{-# LANGUAGE OverloadedStrings #-}

module ContextFree.Grammar.Parser (parseGrammar, grammarP) where

import ContextFree.Grammar (Grammar, GrammarError (..), mkGrammar)
import Control.Applicative hiding (some)
import Data.Char (isSpace)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Functor (void)

type Parser = Parsec GrammarError Text

parseGrammar :: Text -> Either String Grammar
parseGrammar input = case parse grammarP "" input of
  Left e -> Left $ errorBundlePretty e
  Right x -> Right x

grammarP :: Parser Grammar
grammarP = do
  scn
  nonterminals <- symbol "Nonterminals:" *> nonterminalsP
  scn
  terminals <- symbol "Terminals:" *> terminalsP nonterminals
  scn
  start <- symbol "Start:" *> startP nonterminals
  scn
  productions <- productionsP nonterminals terminals
  scn

  case mkGrammar terminals productions start of
    Left err -> fancyFailure . Set.singleton $ ErrorCustom err
    Right grammar -> pure grammar

nonterminalsP :: Parser (HashSet Text)
nonterminalsP = toSet $ symbolP `sepByWithTry` sc

terminalsP :: HashSet Text -> Parser (HashSet Text)
terminalsP nonterminals =
  toSet $
    fromSetP
      (NonterminalsAndTerminalsNotDisjoint . HashSet.singleton)
      (not . (`HashSet.member` nonterminals))
      `sepByWithTry` sc

startP :: HashSet Text -> Parser Text
startP set = fromSetP StartSymbolNotInNonterminals (`HashSet.member` set)

productionsP :: HashSet Text -> HashSet Text -> Parser [(Text, [[Text]])]
productionsP nonterminals terminals = do
  some production
  -- TODO: Check if there are duplicate productions
  -- TODO: Check if productions cover all non-terminals
  where
    production = do
      sc
      lhs <- nonterminalP nonterminals
      sc
      void (symbol "→" <|> symbol "->" <?> "arrow")
      rhs <- (anySymbol `sepByWithTry` sc) `sepByWithTry` (sc *> symbol "|" <?> "or")
      scn
      pure (lhs, rhs)
    anySymbol = fromSetP ProductionRhsNotInSymbols allSymbols
    allSymbols s = s `HashSet.member` nonterminals || s `HashSet.member` terminals

nonterminalP :: HashSet Text -> Parser Text
nonterminalP nonterminals = fromSetP ProductionLhsNotInNonterminals (`HashSet.member` nonterminals) <?> "non-terminal"

symbolP :: Parser Text
symbolP = takeWhile1P (Just "symbol") $ \c -> not $ isSpace c || c `elem` ['#', '|']

-- TODO: Error on duplicates
toSet :: (Hashable a) => Parser [a] -> Parser (HashSet a)
toSet = fmap HashSet.fromList

fromSetP ::
  (Text -> GrammarError) ->
  (Text -> Bool) ->
  Parser Text
fromSetP err set = withPredicate checkInSet symbolP
  where
    checkInSet s
      | set s = Nothing
      | otherwise = Just . Set.singleton $ ErrorCustom $ err s

withPredicate ::
  (MonadParsec e s m) =>
  -- | The check to perform on parsed input
  (a -> Maybe (Set (ErrorFancy e))) ->
  -- | Parser to run
  m a ->
  -- | Resulting parser that performs the check
  m a
withPredicate f p = do
  o <- getOffset
  r <- p
  case f r of
    Nothing -> pure ()
    Just err -> registerParseError $ FancyError o err
  pure r

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

sepByWithTry :: (MonadParsec e s m) => m a -> m b -> m [a]
sepByWithTry p sep = do
  r <- optional (try p)
  case r of
    Nothing -> pure []
    Just x -> (x :) <$> many (try (sep >> p))
