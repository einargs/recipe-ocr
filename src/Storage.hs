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

import Models
import App
import Tesseract

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbRecipe
  name Text
  body Text
  deriving Show
DbImage
  recipe DbRecipeId
  index Int
  ext Text
  data ByteString
  UniqueRecipeIndex recipe index
  deriving Show
DbTag
  name Text
  UniqueName name
  deriving Show
DbTagging
  tag DbTagId
  recipe DbRecipeId
|]

type family RecordBackends (backend :: Type) (models :: [Type]) :: Constraint where
  RecordBackends backend '[model] = (PersistRecordBackend model backend)
  RecordBackends backend (model ': models) = (PersistRecordBackend model backend, RecordBackends backend models)

type Backend (backend :: Type) (models :: [Type]) =
  ( PersistQueryRead backend
  , PersistUniqueRead backend
  , RecordBackends backend models
  , BackendCompatible SqlBackend backend
  ) :: Constraint

migrationIO :: AppEnv -> IO ()
migrationIO AppEnv{envPool} =
  runSqlPool (runMigration migrateAll) envPool

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
    images <- from $ table @DbImage
    where_ (images ^. DbImageRecipe ==. val recipeId)
    pure images
  pure $ Recipe
    { recipeId = RecipeId $ fromSqlKey recipeId
    , recipeName = dbRecipeName
    , recipeBody = dbRecipeBody
    , recipeTags = dbTagName . entityVal <$> tags
    , recipeImages = toRecipeImage . entityVal <$> images
    }
    
{-
  tagEntities <- selectList [DbRecipeTagRecipeId ==. recipeId] []
  imageEntities <- selectList [DbRecipeImageRecipeId ==. recipeId] [Asc DbRecipeImageIndex]
  pure $ Recipe
    { recipeId = RecipeId $ fromSqlKey recipeId
    , recipeName = dbRecipeName
    , recipeBody = dbRecipeBody
    , recipeTags = (dbRecipeTagName . entityVal) <$> tagEntities
    , recipeImages = toRecipeImage . entityVal <$> imageEntities
    }
-}

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

  {-
searchRecipes :: RecipeSearch -> App (Int, [Recipe])
searchRecipes RecipeSearch {..} = runDB $ do
  let options = [LimitTo searchLimit] <> case searchOffset of
        Just offset -> [OffsetBy offset]
        Nothing -> []
      filters = case searchQuery of
        Just str -> [DbRecipeBody `like` T.concat ["%", str, "%"]]
        Nothing -> []
  dbRecipes <- selectList filters options
  totalCount <- count filters
  -- TODO: optimize these into one load using `<-.` to check for the whole
  -- list of IDs.
  recipes <- traverse loadFullRecipe dbRecipes
  pure $ (totalCount, recipes)
-}

getRecipe :: RecipeId -> App (Maybe Recipe)
getRecipe (RecipeId rid) = runDB $ do
  mbRecipe <- getEntity (toSqlKey rid :: Key DbRecipe)
  case mbRecipe of
    Just recipe -> Just <$> loadFullRecipe recipe
    Nothing -> pure Nothing

loadPreImage :: PreImage -> App RecipeImage
loadPreImage PreImage{..} = do
  image <- liftIO $ BS.readFile preImagePath
  pure $ RecipeImage
    { recipeImageExt = preImageExt
    , recipeImageData = image
    }

toDbImages :: DbRecipeId -> [RecipeImage] -> [DbImage]
toDbImages dbRecipeId = imap f
  where f i RecipeImage {..} = DbImage
          { dbImageRecipe = dbRecipeId
          , dbImageIndex = i
          , dbImageExt = recipeImageExt
          , dbImageData = recipeImageData
          }

insertRecipePeripherals
  :: ( MonadIO m
     , Backend backend '[DbTagging, DbTag, DbImage]
     , PersistUniqueWrite backend
     , PersistQueryWrite backend
     )
  => Key DbRecipe -> [Text] -> [RecipeImage] -> ReaderT backend m ()
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
  insertMany_ $ toDbImages dbRecipeId images

insertRecipe :: PreRecipe -> App Recipe
insertRecipe PreRecipe
  { preRecipeName = name
  , preRecipeTags = rawTags
  , preRecipeImages = preImages
  } = do
    imageTexts <- traverse ocr $ preImagePath <$> preImages
    let body = T.intercalate " " imageTexts
    images <- traverse loadPreImage preImages
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
        , recipeImages = images
        }

getRecipeImage :: RecipeId -> Int -> App (Maybe RecipeImage)
getRecipeImage (RecipeId rid) idx = runDB do
  mbDbImage <- getBy $ UniqueRecipeIndex (toSqlKey rid) idx
  pure $ toRecipeImage . entityVal <$> mbDbImage

deleteRecipe :: RecipeId -> App ()
deleteRecipe (RecipeId rid) = runDB do
  delete do
    images <- from $ table @DbImage
    where_ (images ^. DbImageRecipe ==. valkey rid)
    taggings <- from $ table @DbTagging
    where_ (taggings ^. DbTaggingRecipe ==. valkey rid)
    recipes <- from $ table @DbRecipe
    where_ (recipes ^. DbRecipeId ==. valkey rid)

  {-
deleteRecipe (RecipeId rid) = runDB $ do
  deleteWhere [DbRecipeImageRecipeId ==. dbRecipeId]
  deleteWhere [DbRecipeTagRecipeId ==. dbRecipeId]
  delete dbRecipeId
  where
    dbRecipeId :: DbRecipeId
    dbRecipeId = toSqlKey rid
-}

updateRecipe :: RecipeId -> PreRecipe -> App ()
updateRecipe (RecipeId rid) PreRecipe
  { preRecipeName = name
  , preRecipeTags = tags
  , preRecipeImages = preImages
  } = do
  images <- traverse loadPreImage preImages
  imageTexts <- traverse ocr $ preImagePath <$> preImages
  let body = T.intercalate " " imageTexts
      dbRecipeId = toSqlKey rid

  runDB $ do
    replace dbRecipeId $ DbRecipe
      { dbRecipeName = name
      , dbRecipeBody = body
      }
    delete do
      images <- from $ table @DbImage
      where_ (images ^. DbImageRecipe ==. valkey rid)
      taggings <- from $ table @DbTagging
      where_ (taggings ^. DbTaggingRecipe ==. valkey rid)
    insertRecipePeripherals dbRecipeId tags images
    {-
    update dbRecipeId
      [ DbRecipeName =. name
      , DbRecipeBody =. body
      ]
    deleteWhere [DbRecipeTagRecipeId ==. dbRecipeId]
    insertMany_ $ mkTag <$> tags
    deleteWhere [DbRecipeImageRecipeId ==. dbRecipeId]
    insertMany_ $ toDbImages dbRecipeId images
  
  where
    mkTag name = DbRecipeTag
      { dbRecipeTagName = name
      , dbRecipeTagRecipeId = dbRecipeId
      }
    dbRecipeId :: DbRecipeId
    dbRecipeId = toSqlKey rid
-}
