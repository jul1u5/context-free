{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ContextFree.Grammar.Parser (Grammar', parseGrammar, preGrammarP, PreGrammar (..), asGrammar) where

import ContextFree.Grammar
import Control.Applicative hiding (many, some)
import Data.Char (isSpace)
import Data.Functor (void)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec GrammarError Text

parseGrammar :: FilePath -> Text -> Either String Grammar
parseGrammar fp input = case parse preGrammarP fp input of
  Left e -> Left $ errorBundlePretty e
  Right pre -> case asGrammar pre of
    Left err -> Left $ prettyGrammarError err
    Right grammar -> pure grammar

data PreGrammar = PreGrammar
  { preTerminals :: HashSet Text,
    preStart :: Text,
    preProductions :: [(Text, [[Text]])]
  }
  deriving (Show, Eq)

asGrammar :: PreGrammar -> Either GrammarError Grammar
asGrammar PreGrammar {preTerminals, preStart, preProductions} =
  case mkGrammar preTerminals preProductions preStart of
    Left err -> Left err
    Right grammar -> pure grammar

preGrammarP :: Parser PreGrammar
preGrammarP = do
  scn
  nts <- symbol "Nonterminals:" *> nonterminalsP
  scn
  ts <- symbol "Terminals:" *> terminalsP nts
  scn
  start <- symbol "Start:" *> startP nts
  scn
  productions <- productionsP nts ts
  scn
  eof

  pure $
    PreGrammar
      { preTerminals = ts,
        preStart = start,
        preProductions = productions
      }

nonterminalsP :: Parser (HashSet Text)
nonterminalsP = toSet $ symbolP `sepByWithTry` sc

terminalsP :: HashSet Text -> Parser (HashSet Text)
terminalsP nts =
  toSet $
    symbolSetP
      (NonterminalsAndTerminalsNotDisjoint . HashSet.singleton)
      (not . (`HashSet.member` nts))
      `sepByWithTry` sc

startP :: HashSet Text -> Parser Text
startP nts = symbolSetP StartSymbolNotInNonterminals (`HashSet.member` nts)

productionsP :: HashSet Text -> HashSet Text -> Parser [(Text, [[Text]])]
productionsP nts ts = withPredicate coverage $ production `sepByWithTry` scn
  where
    -- FIXME: Maybe this check should belong somewhere else?
    coverage (HashSet.fromList . map fst -> usedNts)
      | unusedNts <- nts `HashSet.difference` usedNts,
        not $ HashSet.null unusedNts =
          Just $ NonterminalsHaveNoProductionRules unusedNts
      | otherwise = Nothing
    -- TODO: Check if there are duplicate productions

    production = do
      lhs <- nonterminalP nts <?> "production LHS"
      void (symbol "→" <|> symbol "->" <?> "arrow")
      rhs <- (anySymbol `sepBy` sc) `sepBy` (symbol "|" <?> "or") <?> "production RHS"
      pure (lhs, rhs)
    anySymbol = symbolSetP ProductionRhsNotInSymbols allSymbols
    allSymbols s = s `HashSet.member` nts || s `HashSet.member` ts

nonterminalP :: HashSet Text -> Parser Text
nonterminalP nts = symbolSetP ProductionLhsNotInNonterminals (`HashSet.member` nts) <?> "non-terminal"

symbolP :: Parser Text
symbolP = takeWhile1P (Just "symbol") (\c -> not $ isSpace c || c `elem` ['#', '|']) <* sc

-- TODO: Error on duplicates
toSet :: (Hashable a) => Parser [a] -> Parser (HashSet a)
toSet = fmap HashSet.fromList

symbolSetP ::
  -- | Error to produce based on the symbol
  (Text -> GrammarError) ->
  -- | Predicate to check
  (Text -> Bool) ->
  Parser Text
symbolSetP err set = do
  withPredicate checkInSet symbolP
  where
    checkInSet s
      | set s = Nothing
      | otherwise = Just $ err s

withPredicate ::
  (MonadParsec e s m) =>
  -- | The check to perform on parsed input
  (a -> Maybe e) ->
  -- | Parser to run
  m a ->
  -- | Resulting parser that performs the check
  m a
withPredicate f p = do
  o <- getOffset
  r <- p
  case f r of
    Nothing -> pure ()
    Just err -> do
      registerParseError $ FancyError o $ Set.singleton $ ErrorCustom err
  pure r

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t' <|> char '\2004')) lineComment empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

sepByWithTry :: (MonadParsec e s m) => m a -> m b -> m [a]
sepByWithTry p sep = do
  r <- optional (try p)
  case r of
    Nothing -> pure []
    Just x -> (x :) <$> many (try (sep *> p))
