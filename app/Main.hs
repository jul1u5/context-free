{-# LANGUAGE OverloadedStrings #-}

module Main where

import App.Config
import App.Helpers
import App.Options qualified as Opt
import ContextFree.Grammar
import ContextFree.Parsing (cyk, prettyCYKTable)
import ContextFree.StrongEquivalence
import ContextFree.Transformations
import Data.Foldable (foldl')
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
  config <- Opt.getConfig

  let grammar = foldl' (flip ($)) config.grammar $ map transformationToFn config.transformations

  output <- case config.operation of
    Nothing ->
      pure $ prettyGrammar grammar
    Just (Parse w) -> do
      cnf <- tryRightShow @AsCNFError $ asCNF grammar
      let table = cyk cnf w
      pure $ prettyCYKTable table w
    Just (Compare otherGrammar) -> do
      let relabelings = equiv grammar otherGrammar
      case relabelings of
        [] -> do
          TIO.hPutStrLn stderr "The grammars are not equivalent."
          exitFailure
        relabeling : _ ->
          pure $ prettyRelabeling relabeling

  TIO.putStrLn output
