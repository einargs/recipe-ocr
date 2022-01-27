module Api
  ( app
  , runApp
  ) where

import Data.Proxy
import Data.ByteString (ByteString)
import Servant.API
import Servant.Server
import Servant.Multipart
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import App
import Models
import qualified Storage as S

type RecipeAPI
  -- Get a recipe
     = Capture "recipeId" RecipeId :> Get '[JSON] RecipeOut
  -- Post a recipe
  :<|> MultipartForm Tmp PreRecipe :> Post '[JSON] RecipeOut
  -- Get the image for a recipe
  :<|> Capture "recipeId" RecipeId :> "images"
       :> Capture "imageIndex" Int :> Get '[OctetStream] ByteString
  -- Put a recipe update
  :<|> Capture "recipeId" RecipeId :> MultipartForm Tmp PreRecipe
       :> PutNoContent
  -- Delete a recipe
  :<|> Capture "recipeId" RecipeId :> DeleteNoContent
  -- Get and search all recipes
  :<|> QueryParam "limit" Int :> QueryParam "offset" Int
    :> QueryParam "query" Text :> Get '[JSON] RecipeSlice

type FullAPI = "api" :> "recipes" :> RecipeAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy

apiServer :: ServerT FullAPI App
apiServer = getRecipe
          :<|> uploadRecipe
          :<|> getRecipeImage
          :<|> putRecipe
          :<|> deleteRecipe
          :<|> searchRecipes
  where
    getRecipe :: RecipeId -> App RecipeOut
    getRecipe = fmap toRecipeOut . unwrapWith404 . S.getRecipe

    getRecipeImage :: RecipeId -> Int -> App ByteString
    getRecipeImage recId idx = fmap recipeImageData
      $ unwrapWith404 $ S.getRecipeImage recId idx

    uploadRecipe :: PreRecipe -> App RecipeOut
    uploadRecipe = fmap toRecipeOut . S.insertRecipe

    putRecipe :: RecipeId -> PreRecipe -> App NoContent
    putRecipe rid rupdate = S.updateRecipe rid rupdate $> NoContent
    
    deleteRecipe :: RecipeId -> App NoContent
    deleteRecipe rid = S.deleteRecipe rid $> NoContent 

    searchRecipes :: Maybe Int -> Maybe Int -> Maybe Text -> App RecipeSlice
    searchRecipes mbLimit mbOffset mbQuery = do
      (count, recipes) <- S.searchRecipes S.RecipeSearch
        { searchOffset = mbOffset
        , searchLimit = fromMaybe 20 mbLimit
        , searchQuery = mbQuery
        }
      pure $ RecipeSlice
        { recipeSliceTotal = count
        , recipeSliceRecipes = toRecipeOut <$> recipes }

    unwrapWith404 :: App (Maybe a) -> App a
    unwrapWith404 m = m >>= \case
      Just v -> pure v
      Nothing -> throwError err404

app :: AppEnv -> Application
app env =
  showRequest
  $ logStdoutDev
  $ simpleCors
  $ serve fullAPI
  $ hoistServer fullAPI (appToHandler env) apiServer
  where
    showRequest :: Middleware
    showRequest app req f = do
      print req
      app req f

runApp :: Config -> IO ()
runApp cfg = withAppEnv cfg $ run (configPort cfg) . app
