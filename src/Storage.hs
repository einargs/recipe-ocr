{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Storage
  ( searchRecipes
  , RecipeSearch(..)
  , getRecipe
  , insertRecipe
  , getRecipeImage
  , updateRecipe
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
import System.FilePath (takeExtension)
import Data.Foldable
import Data.Kind
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.List.Index (imap)

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
  }

searchRecipes :: RecipeSearch -> App [Recipe]
searchRecipes RecipeSearch {..} = runDB $ do
  let options = [LimitTo searchLimit] <> case searchOffset of
        Just offset -> [OffsetBy offset]
        Nothing -> []
      filters = case searchQuery of
        Just str -> [DbRecipeBody `like` T.concat ["%", str, "%"]]
        Nothing -> []
  dbRecipes <- selectList filters options
  traverse loadFullRecipe dbRecipes

getRecipe :: RecipeId -> App (Maybe Recipe)
getRecipe (RecipeId rid) = runDB $ do
  mbRecipe <- getEntity (toSqlKey rid :: Key DbRecipe)
  case mbRecipe of
    Just recipe -> Just <$> loadFullRecipe recipe
    Nothing -> pure Nothing

insertRecipe :: PreRecipe -> App Recipe
insertRecipe PreRecipe
  { preRecipeName = name
  , preRecipeTags = tags
  , preRecipeImagePaths = imagePaths
  } = do
    body <- T.intercalate " " <$> traverse ocr imagePaths
    images <- traverse loadImage imagePaths
    runDB $ do
      dbRecipeId <- insert $ DbRecipe
        { dbRecipeName = name
        , dbRecipeBody = body
        }

      let imgf i RecipeImage {..} = DbRecipeImage
            { dbRecipeImageRecipeId = dbRecipeId
            , dbRecipeImageIndex = i
            , dbRecipeImageExt = recipeImageExt
            , dbRecipeImageData = recipeImageData
            }
          dbImages = imap imgf images
          tagf t = DbRecipeTag
            { dbRecipeTagName = t
            , dbRecipeTagRecipeId = dbRecipeId
            }
          dbTags = tagf <$> tags

      traverse_ insert_ dbImages
      traverse_ insert_ dbTags

      pure $ Recipe
        { recipeId = RecipeId $ fromSqlKey dbRecipeId
        , recipeName = name
        , recipeBody = body
        , recipeTags = tags
        , recipeImages = images
        }
  where
    loadImage path = do
      image <- liftIO $ BS.readFile path
      pure $ RecipeImage
        { recipeImageExt = T.pack $ takeExtension path
        , recipeImageData = image
        }

getRecipeImage :: RecipeId -> Int -> App (Maybe RecipeImage)
getRecipeImage (RecipeId rid) idx = runDB $ do
  mbDbImage <- selectFirst
    [ DbRecipeImageRecipeId ==. toSqlKey rid
    , DbRecipeImageIndex ==. idx] []
  pure $ toRecipeImage . entityVal <$> mbDbImage

updateRecipe :: RecipeId -> RecipeUpdate -> App ()
updateRecipe (RecipeId rid) RecipeUpdate {..} = runDB $ do
  update dbRecipeId
    [ DbRecipeName =. recipeUpdateName
    , DbRecipeBody =. recipeUpdateBody
    ]
  deleteWhere [DbRecipeTagRecipeId ==. dbRecipeId]
  insertMany_ $ mkTag <$> recipeUpdateTags
  where
    mkTag name = DbRecipeTag
      { dbRecipeTagName = name
      , dbRecipeTagRecipeId = dbRecipeId
      }
    dbRecipeId :: DbRecipeId
    dbRecipeId = toSqlKey rid
