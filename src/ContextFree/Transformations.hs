{-# LANGUAGE OverloadedStrings #-}

module ContextFree.Transformations where

import ContextFree.Grammar.Internal
import Control.Monad (join)
import Control.Monad.Trans.State.Strict (State, evalState, runState)
import Control.Monad.Trans.State.Strict qualified as State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMultimap qualified as HashMultimap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (isNothing)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

type CNF =
  Grammar'
    ( Either
        (Symbol 'Terminal)
        (Symbol 'Nonterminal, Symbol 'Nonterminal)
    )

toCNF :: Grammar -> CNF
toCNF = unsafeAsCNF . unit . del . bin . term . start

unsafeAsCNF :: (HasCallStack) => Grammar -> CNF
unsafeAsCNF g@Grammar {productions} =
  g
    { _productions =
        HashMultimap.map
          ( \case
              [SomeNonterminal a, SomeNonterminal b] -> Right (a, b)
              [SomeTerminal t] -> Left t
              _ -> error "impossible"
          )
          productions
    }

-- | Eliminate the start symbol from right-hand sides
start :: Grammar -> Grammar
start g@Grammar {productions, start = oldStart} =
  let newStart = freshSymbolFor g "S"
   in g
        { _productions = HashMultimap.insert newStart [SomeNonterminal oldStart] productions,
          _start = newStart
        }

type TermM = State (HashMap (Symbol 'Terminal) (Symbol 'Nonterminal))

-- | Eliminate rules with nonsolitary terminals
term :: Grammar -> Grammar
term g@Grammar {productions} = g {_productions = additionalProds <> prods}
  where
    (prods, terminalMappings) =
      flip runState HashMap.empty $
        HashMultimap.traverse transform productions

    additionalProds =
      HashMultimap.fromList
        [ (nt, [SomeTerminal t])
          | (t, nt) <- HashMap.toList terminalMappings
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
        case HashMap.lookup t mappings of
          Just nt -> pure nt
          Nothing -> do
            let nt =
                  freshSymbolFor'
                    (HashSet.fromList (HashMap.elems mappings) <> nonterminals g)
                    g._terminals
                    $ "N" <> t.text
            State.modify' $ HashMap.insert t nt
            pure nt

type BinM = State (HashSet (Symbol 'Nonterminal))

-- | Eliminate right-hand sides with more than 2 nonterminals
bin :: Grammar -> Grammar
bin g@Grammar {productions} = g {_productions = productions'}
  where
    productions' =
      flip evalState HashSet.empty $
        fmap (HashMultimap.fromList . join) $
          sequence $
            [ transform 1 lhs rhs
              | (lhs, rhs) <- HashMultimap.toList productions
            ]
    transform :: Int -> Symbol 'Nonterminal -> [SomeSymbol] -> BinM [(Symbol 'Nonterminal, [SomeSymbol])]
    transform count lhs rhs = case rhs of
      [] -> pure [(lhs, rhs)]
      [_] -> pure [(lhs, rhs)]
      [_, _] -> pure [(lhs, rhs)]
      x : xs -> do
        nts <- State.get
        let !lhs' = freshSymbolFor' nts g._terminals $ lhs.text <> T.pack (show count)
        State.modify' $ HashSet.insert lhs'

        let !count' = succ count
        rest <- transform count' lhs' xs
        pure $ (lhs, [x, SomeNonterminal lhs']) : rest

-- | Eliminate ε-rules
del :: Grammar -> Grammar
del g@Grammar {productions} = g {_productions = delProd productions}

delProd :: Productions [SomeSymbol] -> Productions [SomeSymbol]
delProd productions =
  HashMultimap.foldMapWithKey
    (\lhs rhs -> HashMultimap.fromList $ (lhs,) <$> transform rhs)
    productions
  where
    transform :: [SomeSymbol] -> [[SomeSymbol]]
    transform = filter (not . null) . propEps

    propEps = \case
      [] -> pure []
      x : xs -> case x of
        SomeNonterminal nt
          | nt `HashSet.member` nullable productions ->
              [id, (x :)] <*> propEps xs
        _ -> (x :) <$> propEps xs

nullable :: Productions [SomeSymbol] -> HashSet (Symbol 'Nonterminal)
nullable productions = fixpoint step $ HashMultimap.keysSet $ HashMultimap.filter null productions
  where
    step :: HashSet (Symbol 'Nonterminal) -> HashSet (Symbol 'Nonterminal)
    step prev = HashMultimap.keysSet $ HashMultimap.filter (all inPrev) productions
      where
        inPrev (SomeTerminal _) = False
        inPrev (SomeNonterminal nt) = nt `HashSet.member` prev

-- | Eliminate unit rules
unit :: Grammar -> Grammar
unit g@Grammar {productions} = g {_productions = unitProd productions}

unitProd :: Productions [SomeSymbol] -> Productions [SomeSymbol]
unitProd productions =
  HashMultimap.fromList
    [ (a, cs)
      | (a, b) <- HashMultimap.toList unitPairs,
        cs <- HashSet.toList $ productions HashMultimap.! b,
        isNothing $ asSingleNonterminal cs
    ]
  where
    asSingleNonterminal = \case
      [SomeNonterminal nt] -> Just nt
      _ -> Nothing

    oneStepPairs = HashMultimap.mapMaybe asSingleNonterminal productions
    nonterminals' = HashMultimap.keys productions
    reflexive = HashMultimap.fromList $ zip nonterminals' nonterminals'

    unitPairs = fixpoint step $ oneStepPairs `HashMultimap.union` reflexive
    step prev = prev `HashMultimap.compose` prev

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
  | x == y = x
  | otherwise = fixpoint f y
  where
    y = f x
