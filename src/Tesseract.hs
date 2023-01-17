module Tesseract
  ( ocr
  ) where

import System.Process
import System.Exit (ExitCode(..))
import qualified Data.ByteString as BS
import Data.Text (unpack, Text)
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempDirectory)

import Models
import App

ocr :: FilePath -> App Text
ocr imagePath = do
  liftIO $ TIO.putStrLn "RUNNING OCR"
  liftIO $ withSystemTempDirectory "tesseractDir" $ \path -> do
    let procConfig = (shell $ "tesseract " <> imagePath <> " result -l eng") {cwd = Just path}
    (_,_,_, process) <- createProcess procConfig
    ExitSuccess <- waitForProcess process
    TIO.readFile $ path <> "/result.txt"
