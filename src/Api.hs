module Api
  ( runApp
  ) where

import Data.Proxy
import Data.ByteString (ByteString)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Servant.Multipart
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Application.Static (defaultWebAppSettings)
import WaiAppStatic.Types (ssLookupFile, unsafeToPiece, Pieces, LookupResult(..))

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
    :> QueryParams "tags" Text :> QueryParam "query" Text
    :> Get '[JSON] RecipeSlice

type FullAPI =
  "api" :> "recipes" :> RecipeAPI

apiServer :: ServerT RecipeAPI App
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

    searchRecipes :: Maybe Int -> Maybe Int -> [Text]
                  -> Maybe Text -> App RecipeSlice
    searchRecipes mbLimit mbOffset tags mbQuery = do
      (count, recipes) <- S.searchRecipes S.RecipeSearch
        { searchOffset = mbOffset
        , searchLimit = fromMaybe 20 mbLimit
        , searchQuery = mbQuery
        , searchTags = tags
        }
      pure $ RecipeSlice
        { recipeSliceTotal = count
        , recipeSliceRecipes = toRecipeOut <$> recipes }

    unwrapWith404 :: App (Maybe a) -> App a
    unwrapWith404 m = m >>= \case
      Just v -> pure v
      Nothing -> throwError err404

type FileHost = Raw

fileHostServer :: Config -> ServerT FileHost App
fileHostServer Config{webAppDir} = serveDirectoryWith settings where
  base = defaultWebAppSettings webAppDir
  baseLookup = ssLookupFile base
  lookupFile' :: Pieces -> IO LookupResult
  lookupFile' ps = do
    print ps
    initial <- baseLookup ps
    case initial of
      LRNotFound -> baseLookup [unsafeToPiece "index.html"]
      _ -> pure initial
  settings = base { ssLookupFile = lookupFile' }

type FullSite = FullAPI :<|> FileHost

fullSiteServer :: Config -> ServerT FullSite App
fullSiteServer cfg = apiServer :<|> fileHostServer cfg

serverToApp :: HasServer api '[]
            => Proxy api -> ServerT api App -> AppEnv -> Application
serverToApp api server env = 
  showRequest
  $ logStdoutDev
  $ simpleCors
  $ serve api
  $ hoistServer api (appToHandler env) server
  where
    showRequest :: Middleware
    showRequest app req f = do
      print req
      app req f

runApp :: Config -> IO ()
runApp cfg = withAppEnv cfg $ \env -> do
  S.migrationIO env
  run (configPort cfg) $ app env
  where
    app = if serveSiteDir cfg
             then serverToApp (Proxy @FullSite) $ fullSiteServer cfg
             else serverToApp (Proxy @FullAPI) apiServer

