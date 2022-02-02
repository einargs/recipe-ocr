{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Storage
  ( searchRecipes
  , RecipeSearch(..)
  , getRecipe
  , insertRecipe
  , getRecipeImage
  , updateRecipe
  , deleteRecipe
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
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Database.Esqueleto.Experimental as E
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
DbRecipeImage
  recipeId DbRecipeId
  index Int
  ext Text
  data ByteString
  deriving Show
DbRecipeTag
  recipeId DbRecipeId
  name Text
  deriving Show
|]

like :: EntityField record Text -> Text -> Filter record
like field val = Filter field (FilterValue val) (BackendSpecificFilter "like")

type family RecordBackends (backend :: Type) (models :: [Type]) :: Constraint where
  RecordBackends backend '[model] = (PersistRecordBackend model backend)
  RecordBackends backend (model ': models) = (PersistRecordBackend model backend, RecordBackends backend models)

type Backend (backend :: Type) (models :: [Type]) =
  (PersistQueryRead backend, RecordBackends backend models) :: Constraint

toRecipeImage :: DbRecipeImage -> RecipeImage
toRecipeImage DbRecipeImage {dbRecipeImageExt, dbRecipeImageData} =
  RecipeImage {recipeImageExt = dbRecipeImageExt, recipeImageData = dbRecipeImageData}

loadFullRecipe
  :: (MonadIO m, Backend backend '[DbRecipeTag, DbRecipeImage])
  => Entity DbRecipe -> ReaderT backend m Recipe
loadFullRecipe Entity
  { entityKey=recipeId
  , entityVal=DbRecipe{dbRecipeName, dbRecipeBody}
  } = do
  tagEntities <- selectList [DbRecipeTagRecipeId ==. recipeId] []
  imageEntities <- selectList [DbRecipeImageRecipeId ==. recipeId] [Asc DbRecipeImageIndex]
  pure $ Recipe
    { recipeId = RecipeId $ fromSqlKey recipeId
    , recipeName = dbRecipeName
    , recipeBody = dbRecipeBody
    , recipeTags = (dbRecipeTagName . entityVal) <$> tagEntities
    , recipeImages = toRecipeImage . entityVal <$> imageEntities
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
  dbRecipes <- E.select $ E.distinct $ do
      recipes <- recipeQuery
      case searchOffset of
        Just offset -> E.offset $ fromIntegral offset
        Nothing -> pure ()
      E.limit $ fromIntegral searchLimit
      pure recipes
  Just (E.Value totalCount) <- E.selectOne do
    recipes <- recipeQuery
    pure $ E.countDistinct (recipes E.^. DbRecipeId)
  -- TODO: optimize these into one query.
  recipes <- traverse loadFullRecipe dbRecipes
  pure $ (totalCount, recipes)
  where
    recipeQuery :: E.SqlQuery (E.SqlExpr (Entity DbRecipe))
    recipeQuery = do
      (recipes E.:& tags) <- E.from $ E.table @DbRecipe
        `E.innerJoin` E.table @DbRecipeTag
        `E.on` (\(recipes E.:& tags) ->
                  recipes E.^. DbRecipeId E.==. tags E.^. DbRecipeTagRecipeId)
      case searchTags of
        [] -> pure ()
        _ -> E.where_ $ tags E.^. DbRecipeTagName `E.in_` E.valList searchTags
      case searchQuery of
        Nothing -> pure ()
        Just str ->
          let query = (E.%) E.++. E.val str E.++. (E.%) in
          E.where_ (recipes E.^. DbRecipeBody `E.like` query)
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

toDbImages :: DbRecipeId -> [RecipeImage] -> [DbRecipeImage]
toDbImages dbRecipeId = imap f
  where f i RecipeImage {..} = DbRecipeImage
          { dbRecipeImageRecipeId = dbRecipeId
          , dbRecipeImageIndex = i
          , dbRecipeImageExt = recipeImageExt
          , dbRecipeImageData = recipeImageData
          }

insertRecipe :: PreRecipe -> App Recipe
insertRecipe PreRecipe
  { preRecipeName = name
  , preRecipeTags = tags
  , preRecipeImages = preImages
  } = do
    imageTexts <- traverse ocr $ preImagePath <$> preImages
    let body = T.intercalate " " imageTexts
    images <- traverse loadPreImage preImages
    runDB $ do
      dbRecipeId <- insert $ DbRecipe
        { dbRecipeName = name
        , dbRecipeBody = body
        }

      let tagf t = DbRecipeTag
            { dbRecipeTagName = t
            , dbRecipeTagRecipeId = dbRecipeId
            }
          dbTags = tagf <$> tags
          dbImages = toDbImages dbRecipeId images

      traverse_ insert_ dbImages
      traverse_ insert_ dbTags

      pure $ Recipe
        { recipeId = RecipeId $ fromSqlKey dbRecipeId
        , recipeName = name
        , recipeBody = body
        , recipeTags = tags
        , recipeImages = images
        }

getRecipeImage :: RecipeId -> Int -> App (Maybe RecipeImage)
getRecipeImage (RecipeId rid) idx = runDB $ do
  mbDbImage <- selectFirst
    [ DbRecipeImageRecipeId ==. toSqlKey rid
    , DbRecipeImageIndex ==. idx] []
  pure $ toRecipeImage . entityVal <$> mbDbImage

deleteRecipe :: RecipeId -> App ()
deleteRecipe (RecipeId rid) = runDB $ do
  deleteWhere [DbRecipeImageRecipeId ==. dbRecipeId]
  deleteWhere [DbRecipeTagRecipeId ==. dbRecipeId]
  delete dbRecipeId
  where
    dbRecipeId :: DbRecipeId
    dbRecipeId = toSqlKey rid

updateRecipe :: RecipeId -> PreRecipe -> App ()
updateRecipe (RecipeId rid) PreRecipe
  { preRecipeName = name
  , preRecipeTags = tags
  , preRecipeImages = preImages
  } = do
  images <- traverse loadPreImage preImages
  imageTexts <- traverse ocr $ preImagePath <$> preImages
  let body = T.intercalate " " imageTexts

  runDB $ do
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
