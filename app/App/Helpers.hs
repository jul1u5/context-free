module App.Helpers where

import Control.Exception (Exception, displayException)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

tryRight :: Either Text a -> IO a
tryRight (Right x) = pure x
tryRight (Left err) = do
  TIO.hPutStrLn stderr err
  exitFailure

tryRightE :: (Exception e) => Either e a -> IO a
tryRightE (Right x) = pure x
tryRightE (Left err) = do
  hPutStrLn stderr $ displayException err
  exitFailure
