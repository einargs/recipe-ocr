{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Models
  ( RecipeImage(..)
  , Recipe(..)
  , RecipeId(..)
  , ImageHash(..)
  , PreRecipe(..)
  , PreImage(..)
  , RecipeOut(..)
  , RecipeSlice(..)
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
import System.FilePath (takeExtension, takeBaseName)
import Debug.Trace

import App

stripPrefix :: String -> String -> String
stripPrefix prefixStr = lower1 . drop (length prefixStr)
  where
    lower1 (c:cs) = toLower c : cs
    lower1 [] = []


data RecipeImage = RecipeImage
  { recipeImageExt :: !Text
  , recipeImageHash :: !ImageHash
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
  , recipeOutImages = imgUrl <$> recipeImages
  }
  where imgUrl RecipeImage {..} = T.concat
          [ "/api/recipes/"
          , T.pack $ show $ rawRecipeId recipeId
          , "/images/"
          , T.pack $ show $ rawImageHash recipeImageHash
          --, recipeImageExt
          ]

instance ToJSON RecipeOut where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefix "recipeOut" }

data RecipeSlice = RecipeSlice
  -- the total amount of recipes that register for a query
  { recipeSliceTotal :: Int
  -- this specific page of recipes
  , recipeSliceRecipes :: [RecipeOut]
  } deriving (Show, Generic)

instance ToJSON RecipeSlice where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = stripPrefix "recipeSlice" }

data PreImage = PreImage
  { preImageExt :: Text
  , preImagePath :: FilePath
  } deriving Show

imagesFromMultipart :: Text -> MultipartData Tmp -> [PreImage]
imagesFromMultipart name form = 
  toImg <$> filter ((== name) . fdInputName) (files form)
  where
    toImg FileData{..} = PreImage
      { preImageExt = T.pack $ takeExtension $ T.unpack fdFileName
      , preImagePath = fdPayload
      }

data PreRecipe = PreRecipe
  { preRecipeName :: Text
  , preRecipeTags :: [Text]
  , preRecipeImages :: [PreImage]
  } deriving Show

instance FromMultipart Tmp PreRecipe where
  fromMultipart form = do
    -- traceShowM $ files form
    name <- lookupInput "name" form
    rawTags <- lookupInput "tags" form
    pure $ PreRecipe
      { preRecipeName = name
      , preRecipeTags = splitTags rawTags
      , preRecipeImages = imagesFromMultipart "images" form
      }
    where
      splitTags = fmap T.strip . T.splitOn ","

newtype RecipeId = RecipeId { rawRecipeId :: Int64 }
  deriving (Show, Eq, Ord)
  deriving (FromHttpApiData) via (Int64)
  deriving (ToJSON) via (Int64)

newtype ImageHash = ImageHash { rawImageHash :: Int }
  deriving (Show, Eq, Ord)
  deriving (FromHttpApiData) via (Int)
  deriving (ToJSON) via (Int)
