{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import ContextFree.Grammar.Internal (Symbol (UnsafeMkSymbol))
import ContextFree.Grammar.Parser qualified as CF
import ContextFree.Parsing (cyk, showCykTable)
import ContextFree.Transformations qualified as CF
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  file : rest <- getArgs
  let start :: Int = case rest of
        [] -> 0
        [s] -> read s

  source <- TIO.readFile file

  case CF.parseGrammar source of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right g -> do
      let tasks =
            ("(ORIGINAL)", id)
              : drop
                start
                [ ("1.1 (BIN)", CF.bin),
                  ("1.2 (DEL)", CF.del),
                  ("1.3 (UNIT)", CF.unit),
                  ("1.4 (TERM)", CF.term)
                ]

      g' <- flip execStateT g $ forM_ tasks $ \(title, f) -> do
        g' <- gets f
        put g'
        liftIO $ printTask title g'

      let string = map UnsafeMkSymbol $ T.words "1 + 1 1 - 1"
          table = cyk (CF.unsafeAsCNF g') string
      TIO.putStrLn $ showCykTable table string

printTask :: (Show a) => String -> a -> IO ()
printTask title a = do
  putStrLn ""
  putStrLn $ "# " ++ title
  putStrLn ""
  print a
  putStrLn $ replicate 20 '-'
