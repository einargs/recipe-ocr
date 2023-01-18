module Main where

import qualified Data.ByteString as BS
import Options.Applicative
import Data.Text (Text)

import App
import Models
import Api

devConfig :: Parser Config
devConfig = flag' cfg
  (long "dev-defaults"
  <> help "enable development defaults")
  where cfg = Config
          { sqliteFile = "./db/test.db"
          , configPort = 8000
          , webAppDir = "./recipe-site/dist/"
          , serveSiteDir = False
          , envConfig = Development
          }


mainConfig :: Parser Config
mainConfig = Config
  <$> strOption @Text
    (long "db"
    <> metavar "DB_FILE"
    <> value "./recipe.db"
    <> help "sqlite db file")
  <*> option auto
    (long "port"
    <> short 'p'
    <> metavar "PORT"
    <> value 80
    <> help "port to open server on")
  <*> strOption
    (long "site"
    <> short 's'
    <> metavar "SITE_DIR"
    <> value "./site/"
    <> help "directory with the built site")
  <*> flag True False
    (long "no-serve-site"
    <> help "do not serve the static site")
  <*> flag Production Development
    (long "dev"
    <> help "indicates development mode. Currently does nothing.")

config :: Parser Config
config = devConfig <|> mainConfig

main :: IO ()
main = do
  putStrLn "Starting"
  cfg <- execParser opts
  print cfg
  runApp cfg
  where opts = info (config <**> helper)
          (fullDesc <> progDesc "Run a recipe ocr and indexing server")

