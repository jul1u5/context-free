{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

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
import GHC.Records (HasField, getField)

-- | Grammar for a context-free language
--
-- Invariants:
--   1. Non-terminals and terminals are disjoint
--   2. The lhs of each production is a non-terminal
--   3. The rhs of each production is a list of symbols (non-terminals or terminals)
--   4. The start symbol is in 'nonterminals'.
data Grammar' f = UnsafeMkGrammar
  { terminals :: HashSet (Symbol 'Terminal),
    productions :: Productions f,
    start :: Symbol 'Nonterminal
  }
  deriving (Show, Eq)

type Grammar = Grammar' [SomeSymbol]

type Productions f = HashMultimap (Symbol 'Nonterminal) f

pattern Grammar :: HashSet (Symbol 'Terminal) -> Productions f -> Symbol 'Nonterminal -> Grammar' f
pattern Grammar {terminals, productions, start} <- UnsafeMkGrammar terminals productions start

{-# COMPLETE Grammar #-}

-- instance HasField "terminals" (Grammar' f) (HashSet (Symbol 'Terminal)) where
--   getField = (._terminals)

instance HasField "nonterminals" (Grammar' f) (HashSet (Symbol 'Nonterminal)) where
  getField = HashMultimap.keysSet . (.productions)

prettyGrammar :: Grammar -> Text
prettyGrammar g@Grammar {terminals, start, productions} =
  T.unlines $
    [ "Nonterminals: " <> T.unwords (map (.text) $ HashSet.toList g.nonterminals),
      "Terminals: " <> T.unwords (map (.text) $ HashSet.toList terminals),
      "Start: " <> start.text
    ]
      ++ map showProduction (HashMultimap.toGroupedList productions)
  where
    showProduction :: (Symbol 'Nonterminal, [[SomeSymbol]]) -> Text
    showProduction (lhs, rhss) =
      lhs.text <> " -> " <> T.intercalate " | " (map showRhs rhss)

    showRhs rhs = T.unwords $ map showSomeSymbol rhs

    showSomeSymbol (SomeTerminal s) = s.text
    showSomeSymbol (SomeNonterminal s) = s.text

data GrammarError
  = StartSymbolNotInNonterminals {start :: Text}
  | NonterminalsAndTerminalsNotDisjoint {intersection :: HashSet Text}
  | ProductionLhsNotInNonterminals {lhs :: Text}
  | ProductionRhsNotInSymbols {rhs :: Text}
  | NonterminalsHaveNoProductionRules {nts :: HashSet Text}
  deriving (Show, Eq, Ord)

prettyGrammarError :: GrammarError -> String
prettyGrammarError = \case
  StartSymbolNotInNonterminals {start} ->
    "Start symbol " <> show start <> " is not a nonterminal symbol."
  NonterminalsAndTerminalsNotDisjoint {intersection} ->
    "Nonterminal(s) and terminal(s) are not disjoint: " <> intercalate ", " (map show $ HashSet.toList intersection)
  ProductionLhsNotInNonterminals {lhs} ->
    "The symbol " <> show lhs <> " on the left-hand side of a production rule is not a nonterminal symbol."
  ProductionRhsNotInSymbols {rhs} ->
    "The symbol " <> show rhs <> " on the right-hand side of a production rule is neither terminal nor nonterminal."
  NonterminalsHaveNoProductionRules {nts} ->
    "Nonterminal(s) " <> intercalate ", " (map show $ HashSet.toList nts) <> " have no production rules."

instance ShowErrorComponent GrammarError where
  showErrorComponent = prettyGrammarError

mkGrammar :: HashSet Text -> [(Text, [[Text]])] -> Text -> Either GrammarError Grammar
mkGrammar terminals productions start
  | not $ start `HashSet.member` nts =
      Left $ StartSymbolNotInNonterminals start
  | not $ HashSet.null intersection =
      Left $ NonterminalsAndTerminalsNotDisjoint intersection
  | otherwise = do
      productions' <- HashMultimap.fromGroupedList <$> traverse (uncurry checkProduction) productions
      Right $
        UnsafeMkGrammar
          { terminals = HashSet.map UnsafeMkSymbol terminals,
            productions = productions',
            start = UnsafeMkSymbol start
          }
  where
    nts = HashSet.fromList $ map fst productions
    intersection = nts `HashSet.intersection` terminals

    checkProduction :: Text -> [[Text]] -> Either GrammarError (Symbol 'Nonterminal, [[SomeSymbol]])
    checkProduction lhs rhs
      | not $ lhs `HashSet.member` nts = Left $ ProductionLhsNotInNonterminals lhs
      | otherwise = (UnsafeMkSymbol lhs,) <$> traverse (traverse checkSymbol) rhs

    checkSymbol s
      | s `HashSet.member` nts = Right $ SomeNonterminal $ UnsafeMkSymbol s
      | s `HashSet.member` terminals = Right $ SomeTerminal $ UnsafeMkSymbol s
      | otherwise = Left $ ProductionRhsNotInSymbols s

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

instance HasField "text" SomeSymbol Text where
  getField (SomeSymbol _ s) = s.text

freshSymbolFor :: Grammar' f -> Text -> Symbol 'Nonterminal
freshSymbolFor g = freshSymbolFor' g.nonterminals g.terminals

freshSymbolFor' ::
  HashSet (Symbol 'Nonterminal) ->
  HashSet (Symbol 'Terminal) ->
  Text ->
  Symbol 'Nonterminal
freshSymbolFor' nts ts suggestion
  | suggestion `HashSet.member` nts' || suggestion `HashSet.member` ts' =
      freshSymbolFor' nts ts (suggestion <> "'")
  | otherwise = UnsafeMkSymbol suggestion
  where
    nts' = HashSet.map (.text) nts
    ts' = HashSet.map (.text) ts

asTerminal ::
  HashSet (Symbol 'Terminal) ->
  Text ->
  Maybe (Symbol 'Terminal)
asTerminal terminals txt
  | UnsafeMkSymbol txt `HashSet.member` terminals = Just $ UnsafeMkSymbol txt
  | otherwise = Nothing
