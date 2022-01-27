module Main where

import qualified Data.ByteString as BS

import App
import Models
import Api

main :: IO ()
main = do
  putStrLn "Starting"
  runApp Config
    { sqliteFile = "db/test.db"
    , configPort = 8000
    }
