{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module App.Options where

import App.Config qualified as C
import App.Helpers
import ContextFree.Grammar.Parser
import ContextFree.Parsing (TokenizerError, tokenize)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import Options.Applicative
import Text.ParserCombinators.ReadP qualified as R
import Data.Maybe (fromMaybe)

data OperationOpt
  = Parse Text
  | Compare FilePath

data Options = Options
  { input :: Maybe FilePath,
    transformations :: [C.Transformation],
    operation :: Maybe OperationOpt
  }

getConfig :: IO C.Config
getConfig = validate =<< execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

optionsParser :: Parser Options
optionsParser = do
  input <-
    optional $
      option str $
        long "input"
          <> short 'i'
          <> metavar "INPUT"
          <> help "Path to a file containing context-free grammar."

  transformations <-
    fmap join $
      many $
        option transformationReader $
          long "transform"
            <> short 't'
            <> metavar "TRANSFORMATIONS"
            <> help "A sequence of transformations to apply (start, bin, del, unit, term, or cnf)."

  operation <-
    optional $
      hsubparser $
        mconcat
          [ command "parse" $
              info (Parse <$> strArgument (metavar "STRING")) $
                progDesc "Parse a string using the CYK algorithm.",
            command "is-equiv" $
              info (Compare <$> strArgument (metavar "FILE")) $
                progDesc "Check if the resulting grammar is (strongly) equivalent to a grammar in the specified file."
          ]

  pure $ Options {..}

validate :: Options -> IO C.Config
validate opt = do
  grammar <- do
    source <- T.decodeUtf8 <$> maybe BS.getContents BS.readFile opt.input
    tryRight $ parseGrammar (fromMaybe "" opt.input) source & first T.pack

  let transformations = opt.transformations

  operation <- for opt.operation $ \case
    Parse s -> do
      w <- tryRightShow @TokenizerError $ tokenize grammar s
      pure $ C.Parse w
    Compare fp -> do
      source <- T.decodeUtf8 <$> BS.readFile fp
      g <- tryRight $ parseGrammar fp source & first T.pack
      pure $ C.Compare g

  pure $ C.Config {..}

transformationReader :: ReadM [C.Transformation]
transformationReader = eitherReader $ \arg ->
  case R.readP_to_S (transformationsP <* R.eof) arg of
    [] -> Left $ "Cannot parse transformations: " ++ arg
    (t, _) : _ -> Right t
  where
    transformationsP =
      asum
        [ R.string "start" $> C.Start,
          R.string "bin" $> C.Bin,
          R.string "del" $> C.Del,
          R.string "unit" $> C.Unit,
          R.string "term" $> C.Term,
          R.string "cnf" $> C.CNF
        ]
        `R.sepBy1` R.string "|"
