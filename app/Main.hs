module Main where

import qualified Data.ByteString as BS
import System.Environment (getArgs)

import App
import Models
import Api

envFromArgs :: [String] -> EnvConfig
envFromArgs ["prod"] = Production
envFromArgs _ = Development

main :: IO ()
main = do
  putStrLn "Starting"
  env <- envFromArgs <$> getArgs
  runApp Config
    { sqliteFile = "db/test.db"
    , configPort = 8000
    , webAppDir = "./recipe-site/dist/"
    , envConfig = env
    }

