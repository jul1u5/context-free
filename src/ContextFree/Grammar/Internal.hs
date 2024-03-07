{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ContextFree.Grammar.Internal where

import Data.HashMultimap (HashMultimap)
import Data.HashMultimap qualified as HashMultimap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable, hashWithSalt)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (ShowErrorComponent (..))

-- | Grammar for a context-free language
--
-- Invariants:
--   1. Non-terminals and terminals are disjoint
--   2. The lhs of each production is a non-terminal
--   3. The rhs of each production is a list of symbols (non-terminals or terminals)
--   4. The start symbol is in 'nonterminals'.
data Grammar' f = UnsafeMkGrammar
  { _terminals :: HashSet (Symbol 'Terminal),
    _productions :: Productions f,
    _start :: Symbol 'Nonterminal
  }
  deriving (Eq)

type Grammar = Grammar' [SomeSymbol]

type Productions f = HashMultimap (Symbol 'Nonterminal) f

pattern Grammar :: HashSet (Symbol 'Terminal) -> Productions f -> Symbol 'Nonterminal -> Grammar' f
pattern Grammar {terminals, productions, start} <- UnsafeMkGrammar terminals productions start

{-# COMPLETE Grammar #-}

nonterminals :: Grammar' f -> HashSet (Symbol 'Nonterminal)
nonterminals = HashMultimap.keysSet . (._productions)

instance Show Grammar where
  show g@Grammar {terminals, start, productions} =
    unlines $
      [ "Nonterminals: " <> T.unpack (T.unwords $ map (.text) $ HashSet.toList $ nonterminals g),
        "Terminals: " <> T.unpack (T.unwords $ map (.text) $ HashSet.toList terminals),
        "Start: " <> T.unpack start.text
      ]
        ++ map showProduction (HashMultimap.toGroupedList productions)
    where
      showProduction :: (Symbol 'Nonterminal, [[SomeSymbol]]) -> String
      showProduction (lhs, rhss) =
        T.unpack lhs.text <> " -> " <> intercalate " | " (map showRhs rhss)
      showRhs rhs = unwords $ map showSomeSymbol rhs
      showSomeSymbol (SomeTerminal s) = T.unpack s.text
      showSomeSymbol (SomeNonterminal s) = T.unpack s.text

data GrammarError
  = StartSymbolNotInNonterminals {start :: Text}
  | NonterminalsAndTerminalsNotDisjoint {intersection :: HashSet Text}
  | ProductionLhsNotInNonterminals {lhs :: Text}
  | ProductionRhsNotInSymbols {rhs :: Text}
  deriving (Eq, Ord)

instance Show GrammarError where
  show = \case
    StartSymbolNotInNonterminals {start} -> "Start symbol `" <> show start <> "` is not in the non-terminals"
    NonterminalsAndTerminalsNotDisjoint {intersection} -> "Non-terminals and terminals are not disjoint: " <> intercalate ", " (map show $ HashSet.toList intersection)
    ProductionLhsNotInNonterminals {lhs} -> "The symbol on the left side of a production rule " <> show lhs <> " is not in the non-terminals"
    ProductionRhsNotInSymbols {rhs} -> "The symbol on the right side of a production rule " <> show rhs <> " is neither non-terminal nor terminal"

instance ShowErrorComponent GrammarError where
  showErrorComponent = show

mkGrammar :: HashSet Text -> [(Text, [[Text]])] -> Text -> Either GrammarError Grammar
mkGrammar terminals productions start
  | not $ start `HashSet.member` nonterminals' =
      Left $ StartSymbolNotInNonterminals start
  | not $ HashSet.null intersection =
      Left $ NonterminalsAndTerminalsNotDisjoint intersection
  | otherwise = do
      productions' <- HashMultimap.fromGroupedList <$> traverse (uncurry checkProduction) productions
      Right $
        UnsafeMkGrammar
          { _terminals = HashSet.map UnsafeMkSymbol terminals,
            _productions = productions',
            _start = UnsafeMkSymbol start
          }
  where
    nonterminals' = HashSet.fromList $ map fst productions
    intersection = nonterminals' `HashSet.intersection` terminals

    checkProduction :: Text -> [[Text]] -> Either GrammarError (Symbol 'Nonterminal, [[SomeSymbol]])
    checkProduction lhs rhs
      | not $ lhs `HashSet.member` nonterminals' = Left $ ProductionLhsNotInNonterminals lhs
      | otherwise = (UnsafeMkSymbol lhs,) <$> traverse (traverse checkSymbol) rhs

    checkSymbol s
      | s `HashSet.member` nonterminals' = Right $ SomeNonterminal $ UnsafeMkSymbol s
      | s `HashSet.member` terminals = Right $ SomeTerminal $ UnsafeMkSymbol s
      | otherwise = Left $ ProductionRhsNotInSymbols s

freshSymbolFor :: Grammar' f -> Text -> Symbol 'Nonterminal
freshSymbolFor g = freshSymbolFor' (nonterminals g) g._terminals

freshSymbolFor' :: HashSet (Symbol 'Nonterminal) -> HashSet (Symbol 'Terminal) -> Text -> Symbol 'Nonterminal
freshSymbolFor' nts ts suggestion
  | suggestion `HashSet.member` nts' || suggestion `HashSet.member` ts' =
      freshSymbolFor' nts ts (suggestion <> "'")
  | otherwise = UnsafeMkSymbol suggestion
  where
    nts' = HashSet.map (.text) nts
    ts' = HashSet.map (.text) ts

type Symbol :: SymbolKind -> Type
newtype Symbol k = UnsafeMkSymbol {text :: Text}
  deriving (Eq, Ord, Hashable)
  deriving newtype (Show, Read)

data SymbolKind = Nonterminal | Terminal
  deriving (Show, Eq)

data SSymbolKind (k :: SymbolKind) where
  SNonterminal :: SSymbolKind 'Nonterminal
  STerminal :: SSymbolKind 'Terminal

deriving instance Show (SSymbolKind k)

deriving instance Eq (SSymbolKind k)

-- | Existential type for symbols with statically unknown kind (terminal or non-terminal).
data SomeSymbol where
  SomeSymbol :: forall (k :: SymbolKind). SSymbolKind k -> Symbol k -> SomeSymbol

pattern SomeTerminal :: Symbol 'Terminal -> SomeSymbol
pattern SomeTerminal s = SomeSymbol STerminal s

pattern SomeNonterminal :: Symbol 'Nonterminal -> SomeSymbol
pattern SomeNonterminal s = SomeSymbol SNonterminal s

{-# COMPLETE SomeTerminal, SomeNonterminal #-}

instance Show SomeSymbol where
  show (SomeSymbol SNonterminal s) = show s
  show (SomeSymbol STerminal s) = show s

instance Eq SomeSymbol where
  SomeSymbol STerminal s1 == SomeSymbol STerminal s2 = s1 == s2
  SomeSymbol SNonterminal s1 == SomeSymbol SNonterminal s2 = s1 == s2
  _ == _ = False

instance Hashable SomeSymbol where
  hashWithSalt s somesym = hashWithSalt @(Either Text Text) s $ case somesym of
    SomeSymbol SNonterminal nt -> Left nt.text
    SomeSymbol STerminal t -> Right t.text
