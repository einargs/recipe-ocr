{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Models
  ( RecipeImage(..)
  , Recipe(..)
  , RecipeId(..)
  , PreRecipe(..)
  , RecipeOut(..)
  , RecipeUpdate(..)
  , toRecipeOut
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import GHC.Generics
import Data.Char (toLower)
import Data.Aeson
import Servant.API (FromHttpApiData)
import Servant.Multipart
import Data.List.Index (imap)

import App

stripPrefix :: String -> String -> String
stripPrefix prefixStr = lower1 . drop (length prefixStr)
  where
    lower1 (c:cs) = toLower c : cs
    lower1 [] = []


data RecipeImage = RecipeImage
  { recipeImageExt :: !Text
  , recipeImageData :: !ByteString
  }
  deriving Show

data Recipe = Recipe
  { recipeId :: RecipeId
  , recipeName :: Text
  , recipeBody :: Text
  , recipeTags :: [Text]
  , recipeImages :: [RecipeImage]
  }
  deriving (Show)

-- | Recipe type that is returned as part of APIs.
data RecipeOut = RecipeOut
  { recipeOutId :: RecipeId
  , recipeOutName :: Text
  , recipeOutBody :: Text
  , recipeOutTags :: [Text]
  -- | URLs to the images; uses index.
  , recipeOutImages :: [Text]
  } deriving (Show, Generic)

toRecipeOut :: Recipe -> RecipeOut
toRecipeOut Recipe {..} = RecipeOut
  { recipeOutId = recipeId
  , recipeOutName = recipeName
  , recipeOutBody = recipeBody
  , recipeOutTags = recipeTags
  , recipeOutImages = imap imgUrl recipeImages
  }
  where imgUrl i RecipeImage {..} = T.concat
          [ "/recipes/"
          , T.pack $ show $ rawRecipeId recipeId
          , "/images/"
          , T.pack $ show i
          , "."
          , recipeImageExt
          ]

instance ToJSON RecipeOut where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefix "recipeOut" }

data PreRecipe = PreRecipe
  { preRecipeName :: Text
  , preRecipeTags :: [Text]
  , preRecipeImagePaths :: [FilePath]
  } deriving Show

data RecipeIn = RecipeIn
  { recipeInName :: Text
  , recipeInTags :: [Text]
  , recipeInImageNames :: [Text]
  } deriving (Show, Generic)

instance FromJSON RecipeIn where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefix "recipeIn" }

instance FromMultipart Tmp PreRecipe where
  fromMultipart form = do
    recipeTxt <- lookupInput "recipe" form
    RecipeIn {..} <- case tdecode recipeTxt of
      Nothing -> Left "JSON in input \"recipe\" did not decode to RecipeIn."
      Just v -> Right v
    let f = fmap fdPayload . flip lookupFile form
    imagePaths <- traverse f recipeInImageNames
    pure $ PreRecipe
      { preRecipeName = recipeInName
      , preRecipeTags = recipeInTags
      , preRecipeImagePaths = imagePaths
      }
    where tdecode = decode . toLazyByteString . encodeUtf8Builder

data RecipeUpdate = RecipeUpdate
  { recipeUpdateName :: Text
  , recipeUpdateTags :: [Text]
  , recipeUpdateBody :: Text
  } deriving (Show, Generic)

instance FromJSON RecipeUpdate where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = stripPrefix "recipeUpdate" }

newtype RecipeId = RecipeId { rawRecipeId :: Int64 }
  deriving (Show, Eq, Ord)
  deriving (FromHttpApiData) via (Int64)
  deriving (ToJSON) via (Int64)
