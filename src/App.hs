module App
  ( App
  , appToHandler
  , MonadIO
  , liftIO
  , MonadReader
  , asks
  , MonadError
  , throwError
  , Config(..)
  , AppEnv(..)
  , initAppEnv
  , closeAppEnv
  , withAppEnv
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Logger
import Control.Monad.Except
import Control.Exception (bracket)
import qualified System.IO as IO
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Data.Text (Text)
import Servant.Server
import qualified Data.Pool as Pool

data Config = Config
  { sqliteFile :: Text
  , configPort :: Int
  }

data AppEnv = AppEnv
  { envPool :: ConnectionPool
  }

newtype App a = App
  { unApp :: ReaderT AppEnv Handler a
  } deriving
  (Functor, Applicative, Monad, MonadReader AppEnv, MonadIO, MonadError ServerError)

initAppEnv :: Config -> IO AppEnv
initAppEnv Config {..} = do
  pool <- runStdoutLoggingT $ createSqlitePool sqliteFile 1
  pure AppEnv
    { envPool = pool
    }

-- | Clean up any resources in the app environment before shutdown.
closeAppEnv :: AppEnv -> IO ()
closeAppEnv AppEnv {envPool} = do
  Pool.destroyAllResources envPool

-- | Bracket the initialization of the app environment.
withAppEnv :: Config -> (AppEnv -> IO a) -> IO a
withAppEnv cfg f = bracket (initAppEnv cfg) closeAppEnv f

-- | Convert an @App@ monad to a @Handler@.
appToHandler :: AppEnv -> App a -> Handler a
appToHandler env (App m) = runReaderT m env
