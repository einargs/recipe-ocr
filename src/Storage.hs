{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Storage
  ( searchRecipes
  , RecipeSearch(..)
  , getRecipe
  , insertRecipe
  , getRecipeImage
  , updateRecipe
  , deleteRecipe
  , migrationIO
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (asks)
import Data.Int (Int64)
import System.Process
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Kind
import qualified Database.Persist as P
-- import Database.Persist.Sql
-- import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Esqueleto.Experimental
import Data.List.Index (imap)
import Debug.Trace (traceShowM)
import Data.Hashable (hash)

import Models
import App
import Tesseract

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbRecipe
  name Text
  body Text
  deriving Show
DbImage
  ext Text
  body Text
  data ByteString
  hash Int
  UniqueImageHash hash
  deriving Show
DbImageIdx
  recipe DbRecipeId OnDeleteCascade
  index Int
  image DbImageId OnDeleteCascade
  UniqueRecipeIndex recipe index
  deriving Show
DbTag
  name Text
  UniqueName name
  deriving Show
DbTagging
  tag DbTagId OnDeleteCascade
  recipe DbRecipeId OnDeleteCascade
|]

type family RecordBackends (backend :: Type) (models :: [Type]) :: Constraint where
  RecordBackends backend '[model] = (PersistRecordBackend model backend)
  RecordBackends backend (model ': models) = (PersistRecordBackend model backend, RecordBackends backend models)

type Backend (backend :: Type) (models :: [Type]) =
  ( PersistQueryRead backend
  , PersistUniqueRead backend
  , PersistQueryWrite backend
  , PersistUniqueWrite backend
  , RecordBackends backend models
  , BackendCompatible SqlBackend backend
  ) :: Constraint

migrationIO :: AppEnv -> IO ()
migrationIO AppEnv{envPool} =
  flip runSqlPool envPool $ runMigration do
    migrateAll
    -- addMigration automatically adds ; to the end
    addMigration False "CREATE VIRTUAL TABLE IF NOT EXISTS fts_idx USING fts5(\
      \body, content='db_recipe', content_rowid='id')"
    triggerMigration "db_recipe_ai" "INSERT"
      "INSERT INTO fts_idx(rowid, body) VALUES (new.id, new.body);"
    triggerMigration "db_recipe_ad" "DELETE"
      "INSERT INTO fts_idx(fts_idx, rowid, body) VALUES ('DELETE', new.id, new.body);"
    triggerMigration "db_recipe_au" "UPDATE"
      "INSERT INTO fts_idx(fts_idx, rowid, body) VALUES ('DELETE', new.id, new.body);\n\
      \INSERT INTO fts_idx(rowid, body) VALUES (new.id, new.body);"
  where
    triggerMigration name form body = addMigration False $ T.concat
      [ "CREATE TRIGGER IF NOT EXISTS ", name, " AFTER ", form
      , " ON db_recipe BEGIN\n", body, "\nEND" ]
      

toRecipeImage :: DbImage -> RecipeImage
toRecipeImage DbImage {dbImageData, dbImageExt} =
  RecipeImage {recipeImageExt = dbImageExt, recipeImageData = dbImageData}

loadFullRecipe
  :: (MonadIO m, Backend backend '[DbTagging, DbTag, DbImage])
  => Entity DbRecipe -> ReaderT backend m Recipe
loadFullRecipe Entity
  { entityKey=recipeId
  , entityVal=DbRecipe{dbRecipeName, dbRecipeBody}
  } = do
  tags <- select do
    (taggings :& tags) <- from $ table @DbTagging
      `innerJoin` table @DbTag
      `on` (\(taggings :& tags) ->
        taggings ^. DbTaggingTag ==. tags ^. DbTagId)
    where_ (taggings ^. DbTaggingRecipe ==. val recipeId)
    pure tags
  images <- select do
    (imageIdx :& images) <-
      from $ table @DbImageIdx
      `innerJoin` table @DbImage
      `on` (\(imageIdx :& images) ->
        imageIdx ^. DbImageIdxImage ==. images ^. DbImageId)
    where_ $ imageIdx ^. DbImageIdxRecipe ==. val recipeId
    orderBy [ asc (imageIdx ^. DbImageIdxIndex) ]
    pure images
  pure $ Recipe
    { recipeId = RecipeId $ fromSqlKey recipeId
    , recipeName = dbRecipeName
    , recipeBody = dbRecipeBody
    , recipeTags = dbTagName . entityVal <$> tags
    , recipeImages = toRecipeImage . entityVal <$> images
    }

runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- asks envPool
  liftIO $ runSqlPool query pool

data RecipeSearch = RecipeSearch
  { searchOffset :: Maybe Int
  , searchLimit :: Int
  , searchQuery :: Maybe Text
  , searchTags :: [Text]
  }

searchRecipes :: RecipeSearch -> App (Int, [Recipe])
searchRecipes RecipeSearch{..} = runDB $ do
  dbRecipes <- select $ distinct $ do
      recipes <- recipeQuery
      case searchOffset of
        Just o -> offset $ fromIntegral o
        Nothing -> pure ()
      limit $ fromIntegral searchLimit
      pure recipes
  mbTotalCount <- selectOne do
    recipes <- recipeQuery
    pure $ countDistinct (recipes ^. DbRecipeId)
  let totalCount = case mbTotalCount of
                     Just (Value tc) -> tc
                     Nothing -> 0
  -- TODO: optimize these into one query.
  recipes <- traverse loadFullRecipe dbRecipes
  pure $ (totalCount, recipes)
  where
    recipeQuery :: SqlQuery (SqlExpr (Entity DbRecipe))
    recipeQuery = do
      (recipes :& taggings :& tags) <- from $ table @DbRecipe
        `innerJoin` table @DbTagging
        `on` (\(recipes :& taggings) ->
          recipes ^. DbRecipeId ==. taggings ^. DbTaggingRecipe)
        `innerJoin` table @DbTag
        `on` (\(_ :& taggings :& tags) ->
          taggings ^. DbTaggingTag ==. tags ^. DbTagId)
      groupBy (taggings ^. DbTaggingRecipe)
      case searchTags of
        [] -> pure ()
        _ -> do
          where_ $ tags ^. DbTagName `in_` valList searchTags
          let len = val $ length searchTags
          having $ (count $ taggings ^. DbTaggingRecipe) ==. len
      case searchQuery of
        Nothing -> pure ()
        Just str ->
          let query = (%) ++. val str ++. (%) in
          where_ (recipes ^. DbRecipeBody `like` query)
      pure recipes

getRecipe :: RecipeId -> App (Maybe Recipe)
getRecipe (RecipeId rid) = runDB $ do
  mbRecipe <- getEntity (toSqlKey rid :: Key DbRecipe)
  case mbRecipe of
    Just recipe -> Just <$> loadFullRecipe recipe
    Nothing -> pure Nothing

-- | Convert a PreImage to a DbImage. If there is not already an image with a
-- matching hash in the database, it will do ocr and insert a new DbImage entry.
loadPreImage :: PreImage -> App (Entity DbImage)
loadPreImage PreImage{..} = do
  imageData <- liftIO $ BS.readFile preImagePath
  let imageHash = hash imageData
  mbImage <- runDB $ selectOne do
    images <- from $ table @DbImage
    where_ $ images ^. DbImageHash ==. val imageHash
    pure images
  case mbImage of
    Just img -> pure img
    Nothing -> do
      body <- ocr preImagePath
      let img = DbImage
                  { dbImageExt = preImageExt
                  , dbImageBody = body
                  , dbImageHash = imageHash
                  , dbImageData = imageData
                  }
      key <- runDB $ insert img
      pure $ Entity key img

-- | Converts a list of DbImages to the DbImageIdx entries to link them to the
-- recipe.
toDbImageIdx :: DbRecipeId -> [Entity DbImage] -> [DbImageIdx]
toDbImageIdx dbRecipeId = imap f
  where f i Entity {entityKey} = DbImageIdx
          { dbImageIdxRecipe = dbRecipeId
          , dbImageIdxIndex = i
          , dbImageIdxImage = entityKey
          }

-- | This inserts the tags and tagging stuff, as well as inserts the
-- DbRecipeIdx.
insertRecipePeripherals
  :: ( MonadIO m
     , Backend backend '[DbTagging, DbTag, DbImage]
     )
  => Key DbRecipe -> [Text] -> [Entity DbImage] -> ReaderT backend m ()
insertRecipePeripherals dbRecipeId rawTags images = do
  let tagf t = DbTag
        { dbTagName = t
        }
      taggingsf tagId = DbTagging
        { dbTaggingRecipe = dbRecipeId
        , dbTaggingTag = tagId
        }

  putMany $ tagf <$> rawTags
  insertSelect do
    tags <- from $ table @DbTag
    where_ (tags ^. DbTagName `in_` valList rawTags)
    pure $ DbTagging <# (tags ^. DbTagId) <&> val dbRecipeId
  putMany $ toDbImageIdx dbRecipeId images

-- | Delete images unreferenced by a DbImageIdx record.
deleteOrphanImages
  :: ( MonadIO m
     , Backend backend '[DbImage, DbImageIdx]
     )
  => ReaderT backend m ()
deleteOrphanImages = do
  delete do
    images <- from $ table @DbImage
    where_ $ images ^. DbImageId `notIn` subList_select do
      imageIdx <- from $ table @DbImageIdx
      pure $ imageIdx ^. DbImageIdxImage

insertRecipe :: PreRecipe -> App Recipe
insertRecipe PreRecipe
  { preRecipeName = name
  , preRecipeTags = rawTags
  , preRecipeImages = preImages
  } = do
    images <- traverse loadPreImage preImages
    let body = T.intercalate " " $ dbImageBody . entityVal <$> images
    runDB do
      dbRecipeId <- insert $ DbRecipe
        { dbRecipeName = name
        , dbRecipeBody = body
        }

      insertRecipePeripherals dbRecipeId rawTags images

      pure $ Recipe
        { recipeId = RecipeId $ fromSqlKey dbRecipeId
        , recipeName = name
        , recipeBody = body
        , recipeTags = rawTags
        , recipeImages = toRecipeImage . entityVal <$> images
        }

getRecipeImage :: RecipeId -> Int -> App (Maybe RecipeImage)
getRecipeImage (RecipeId rid) idx = runDB do
  mbDbImage <- selectOne do
    (images :& imageIdx) <-
      from $ table @DbImage
      `innerJoin` table @DbImageIdx
      `on` (\(images :& imageIdx) ->
        images ^. DbImageId ==. imageIdx ^. DbImageIdxImage)
    where_ $ imageIdx ^. DbImageIdxIndex ==. val idx
      &&. imageIdx ^. DbImageIdxRecipe ==. valkey rid
    pure images

  pure $ toRecipeImage . entityVal <$> mbDbImage

deleteRecipe :: RecipeId -> App ()
deleteRecipe (RecipeId rid) = runDB do
  delete do
    imageIdx <- from $ table @DbImageIdx
    where_ $ imageIdx ^. DbImageIdxRecipe ==. valkey rid
  deleteOrphanImages
  delete do
    taggings <- from $ table @DbTagging
    where_ (taggings ^. DbTaggingRecipe ==. valkey rid)
  deleteKey $ toSqlKey @DbRecipe rid

updateRecipe :: RecipeId -> PreRecipe -> App ()
updateRecipe (RecipeId rid) PreRecipe
  { preRecipeName = name
  , preRecipeTags = tags
  , preRecipeImages = preImages
  } = do
  images <- traverse loadPreImage preImages
  let body = T.intercalate " " $ dbImageBody . entityVal <$> images
      dbRecipeId = toSqlKey rid

  runDB $ do
    replace dbRecipeId $ DbRecipe
      { dbRecipeName = name
      , dbRecipeBody = body
      }
    -- NOTE: I should be able to avoid this delete
    -- using putMany but I'm not sure how.
    delete do
      imageIdx <- from $ table @DbImageIdx
      where_ (imageIdx ^. DbImageIdxRecipe ==. valkey rid)
    delete do
      taggings <- from $ table @DbTagging
      where_ (taggings ^. DbTaggingRecipe ==. valkey rid)
    insertRecipePeripherals dbRecipeId tags images
