{-# LANGUAGE OverloadedStrings #-}

module ContextFree.Transformations where

import ContextFree.Grammar.Internal
import Control.Exception (Exception, displayException)
import Control.Monad (join)
import Control.Monad.Trans.State.Strict (State, evalState, runState)
import Control.Monad.Trans.State.Strict qualified as State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashMultimap qualified as HMM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (isNothing)
import Data.Text qualified as T

type CNF =
  Grammar'
    ( Either
        (Symbol 'Terminal)
        (Symbol 'Nonterminal, Symbol 'Nonterminal)
    )

toCNF :: Grammar -> CNF
toCNF = fromRight . asCNF . unit . del . bin . term . start
  where
    fromRight = either (error . show) id

newtype AsCNFError
  = NonCNFRule (Symbol 'Nonterminal, [SomeSymbol])
  deriving (Show)

prettyAsCNFError :: AsCNFError -> String
prettyAsCNFError (NonCNFRule (lhs, rhs)) =
  "Non-CNF rule: " <> T.unpack lhs.text <> " -> " <> T.unpack (T.unwords $ map (.text) rhs)

instance Exception AsCNFError where
  displayException = prettyAsCNFError

asCNF :: Grammar -> Either AsCNFError CNF
asCNF g@Grammar {productions} = do
  productions' <-
    HMM.traverseWithKey
      ( \lhs -> \case
          [SomeNonterminal a, SomeNonterminal b] -> pure $ Right (a, b)
          [SomeTerminal t] -> pure $ Left t
          rhs -> Left $ NonCNFRule (lhs, rhs)
      )
      productions
  pure $
    UnsafeMkGrammar
      { terminals = g.terminals,
        productions = productions',
        start = g.start
      }

fromCNF :: CNF -> Grammar
fromCNF g@Grammar {productions} =
  UnsafeMkGrammar
    { terminals = g.terminals,
      productions = productions',
      start = g.start
    }
  where
    productions' =
      HMM.map
        ( \case
            Left t -> [SomeTerminal t]
            Right (a, b) -> [SomeNonterminal a, SomeNonterminal b]
        )
        productions

-- | Eliminate the start symbol from right-hand sides
start :: Grammar -> Grammar
start g@Grammar {productions, start = oldStart} =
  let newStart = freshSymbolFor g "S"
   in UnsafeMkGrammar
        { terminals = g.terminals,
          productions = HMM.insert newStart [SomeNonterminal oldStart] productions,
          start = newStart
        }

type TermM = State (HashMap (Symbol 'Terminal) (Symbol 'Nonterminal))

-- | Eliminate rules with nonsolitary terminals
term :: Grammar -> Grammar
term g@Grammar {productions} =
  UnsafeMkGrammar
    { terminals = g.terminals,
      productions = additionalProds <> prods,
      start = g.start
    }
  where
    (prods, terminalMappings) =
      flip runState HM.empty $
        HMM.traverse transform productions

    additionalProds =
      HMM.fromList
        [ (nt, [SomeTerminal t])
          | (t, nt) <- HM.toList terminalMappings
        ]

    transform :: [SomeSymbol] -> TermM [SomeSymbol]
    transform = \case
      [s] -> pure [s]
      rhs -> map SomeNonterminal <$> replaceTerminals rhs

    replaceTerminals :: [SomeSymbol] -> TermM [Symbol 'Nonterminal]
    replaceTerminals = traverse $ \case
      SomeNonterminal nt -> pure nt
      SomeTerminal t -> do
        mappings <- State.get
        case HM.lookup t mappings of
          Just nt -> pure nt
          Nothing -> do
            let nt =
                  freshSymbolFor'
                    (HS.fromList (HM.elems mappings) <> g.nonterminals)
                    g.terminals
                    $ "N" <> t.text
            State.modify' $ HM.insert t nt
            pure nt

type BinM = State (HashSet (Symbol 'Nonterminal))

-- | Eliminate right-hand sides with more than 2 nonterminals
bin :: Grammar -> Grammar
bin g@Grammar {productions} =
  UnsafeMkGrammar
    { terminals = g.terminals,
      productions = productions',
      start = g.start
    }
  where
    productions' =
      flip evalState HS.empty $
        fmap (HMM.fromList . join) $
          sequence $
            [ transform 1 lhs rhs
              | (lhs, rhs) <- HMM.toList productions
            ]
    transform :: Int -> Symbol 'Nonterminal -> [SomeSymbol] -> BinM [(Symbol 'Nonterminal, [SomeSymbol])]
    transform count lhs rhs = case rhs of
      [] -> pure [(lhs, rhs)]
      [_] -> pure [(lhs, rhs)]
      [_, _] -> pure [(lhs, rhs)]
      x : xs -> do
        nts <- State.get
        let !lhs' = freshSymbolFor' nts g.terminals $ lhs.text <> T.pack (show count)
        State.modify' $ HS.insert lhs'

        let !count' = succ count
        rest <- transform count' lhs' xs
        pure $ (lhs, [x, SomeNonterminal lhs']) : rest

-- | Eliminate ε-rules
del :: Grammar -> Grammar
del g =
  UnsafeMkGrammar
    { terminals = g.terminals,
      productions = delProd g.productions,
      start = g.start
    }

delProd :: Productions [SomeSymbol] -> Productions [SomeSymbol]
delProd productions =
  HMM.foldMapWithKey
    (\lhs rhs -> HMM.fromList $ (lhs,) <$> transform rhs)
    productions
  where
    transform :: [SomeSymbol] -> [[SomeSymbol]]
    transform = filter (not . null) . propEps

    propEps = \case
      [] -> pure []
      x : xs -> case x of
        SomeNonterminal nt
          | nt `HS.member` nullable productions ->
              [id, (x :)] <*> propEps xs
        _ -> (x :) <$> propEps xs

nullable :: Productions [SomeSymbol] -> HashSet (Symbol 'Nonterminal)
nullable productions = fixpoint step $ HMM.keysSet $ HMM.filter null productions
  where
    step :: HashSet (Symbol 'Nonterminal) -> HashSet (Symbol 'Nonterminal)
    step prev = HMM.keysSet $ HMM.filter (all inPrev) productions
      where
        inPrev (SomeTerminal _) = False
        inPrev (SomeNonterminal nt) = nt `HS.member` prev

-- | Eliminate unit rules
unit :: Grammar -> Grammar
unit g =
  UnsafeMkGrammar
    { terminals = g.terminals,
      productions = unitProd g.productions,
      start = g.start
    }

unitProd :: Productions [SomeSymbol] -> Productions [SomeSymbol]
unitProd productions =
  HMM.fromList
    [ (a, cs)
      | (a, b) <- HMM.toList unitPairs,
        cs <- HS.toList $ productions HMM.! b,
        isNothing $ asSingleNonterminal cs
    ]
  where
    asSingleNonterminal = \case
      [SomeNonterminal nt] -> Just nt
      _ -> Nothing

    oneStepPairs = HMM.mapMaybe asSingleNonterminal productions
    nonterminals' = HMM.keys productions
    reflexive = HMM.fromList $ zip nonterminals' nonterminals'

    unitPairs = fixpoint step $ oneStepPairs `HMM.union` reflexive
    step prev = prev `HMM.compose` prev

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
  | x == y = x
  | otherwise = fixpoint f y
  where
    y = f x
