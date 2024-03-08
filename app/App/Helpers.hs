module App.Helpers where

import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)
import Data.Text (Text)

tryRight :: Either Text a -> IO a
tryRight (Right x) = pure x
tryRight (Left err) = do
  TIO.hPutStrLn stderr err
  exitFailure


tryRightShow :: Show e => Either e a -> IO a
tryRightShow (Right x) = pure x
tryRightShow (Left err) = do
  hPrint stderr err
  exitFailure
