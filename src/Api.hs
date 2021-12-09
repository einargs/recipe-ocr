module Api
  ( app
  , runApp
  ) where

import Data.Proxy
import Data.ByteString (ByteString)
import Servant.API
import Servant.Server
import Servant.Multipart
import Network.Wai.Handler.Warp (run)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import App
import Models
import qualified Storage as S

type RecipeAPI
  -- Get a recipe
     = Capture "recipeId" RecipeId :> Get '[JSON] RecipeOut
  -- Get the image for a recipe
  :<|> Capture "recipeId" RecipeId :> "images"
       :> Capture "imageIndex" Int :> Get '[OctetStream] ByteString
  -- Post a recipe
  :<|> MultipartForm Tmp PreRecipe :> Post '[JSON] RecipeOut
  -- Put a recipe update
  :<|> Capture "recipeId" RecipeId :> ReqBody '[JSON] RecipeUpdate
       :> Put '[JSON] NoContent
  -- Get and search all recipes
  :<|> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "query" Text :> Get '[JSON] [RecipeOut]

type FullAPI = "recipes" :> RecipeAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy

apiServer :: ServerT FullAPI App
apiServer = getRecipe
          :<|> getRecipeImage
          :<|> uploadRecipe
          :<|> putRecipe
          :<|> searchRecipes
  where
    getRecipe :: RecipeId -> App RecipeOut
    getRecipe = fmap toRecipeOut . unwrapWith404 . S.getRecipe

    getRecipeImage :: RecipeId -> Int -> App ByteString
    getRecipeImage recId idx = fmap recipeImageData
      $ unwrapWith404 $ S.getRecipeImage recId idx

    uploadRecipe :: PreRecipe -> App RecipeOut
    uploadRecipe = fmap toRecipeOut . S.insertRecipe

    putRecipe :: RecipeId -> RecipeUpdate -> App NoContent
    putRecipe rid rupdate = S.updateRecipe rid rupdate $> NoContent

    searchRecipes :: Maybe Int -> Maybe Int -> Maybe Text -> App [RecipeOut]
    searchRecipes mbLimit mbOffset mbQuery =
      fmap toRecipeOut <$> S.searchRecipes S.RecipeSearch
        { searchOffset = mbOffset
        , searchLimit = fromMaybe 20 mbLimit
        , searchQuery = mbQuery
        }

    unwrapWith404 :: App (Maybe a) -> App a
    unwrapWith404 m = m >>= \case
      Just v -> pure v
      Nothing -> throwError err404

app :: AppEnv -> Application
app env = serve fullAPI $ hoistServer fullAPI (appToHandler env) apiServer

runApp :: Config -> IO ()
runApp cfg = withAppEnv cfg $ run (configPort cfg) . app
