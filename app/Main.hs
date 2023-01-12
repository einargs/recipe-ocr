module Main where

import qualified Data.ByteString as BS
import Options.Applicative

import App
import Models
import Api

config :: Parser Config
config = Config
  <$> strOption
    (long "db"
    <> metavar "DB_FILE"
    <> value "./db/test.db"
    <> help "sqlite db file")
  <*> option auto
    (long "port"
    <> short 'p'
    <> metavar "PORT"
    <> value 8000
    <> help "port to open server on")
  <*> strOption
    (long "site"
    <> short 's'
    <> metavar "SITE_DIR"
    <> value "./recipe-site/dist/"
    <> help "directory with the built site")
  <*> flag Production Development
    (long "dev"
    <> help "only serve the api for development")


main :: IO ()
main = do
  putStrLn "Starting"
  cfg <- execParser opts
  runApp cfg
  where opts = info (config <**> helper)
          (fullDesc <> progDesc "Run a recipe ocr and indexing server")

