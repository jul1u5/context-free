{-# LANGUAGE OverloadedStrings #-}

module ContextFree.Transformations where

import ContextFree.Grammar.Internal
import Control.Monad.Trans.State.Strict (State, get, gets, modify', put, runState)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMultimap qualified as HashMultimap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T

-- | Eliminate the start symbol from right-hand sides
start :: Text -> Grammar -> Maybe Grammar
start newStart g@Grammar {terminals, start = oldStart, productions}
  | UnsafeMkSymbol newStart `HashSet.member` nonterminals g = Nothing
  | UnsafeMkSymbol newStart `HashSet.member` terminals = Nothing
  | otherwise =
      let newStart' = UnsafeMkSymbol newStart
       in Just $
            UnsafeMkGrammar
              { terminals,
                start = newStart',
                productions = HashMultimap.insert newStart' [SomeNonterminal oldStart] productions
              }

type TermM = State (HashMap (Symbol 'Terminal) (Symbol 'Nonterminal))

-- | Eliminate rules with nonsolitary terminals
term :: Grammar -> Grammar
term g@Grammar {productions} =
  g {productions = additionalProds <> prods}
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
        gets (HashMap.lookup t) >>= \case
          Just nt -> pure nt
          Nothing -> do
            let nt = UnsafeMkSymbol $ "N" <> t.text
            modify' $ HashMap.insert t nt
            pure nt

-- | Eliminate right-hand sides with more than 2 nonterminals
bin :: Grammar -> Grammar
bin g@Grammar {productions} =
  g {productions = binProd productions}

binProd :: Productions -> Productions
binProd =
  HashMultimap.foldMapWithKey
    (\lhs rhs -> HashMultimap.fromList $ transform 1 lhs rhs)
  where
    transform :: Int -> Symbol 'Nonterminal -> [SomeSymbol] -> [(Symbol 'Nonterminal, [SomeSymbol])]
    transform !count !lhs rhs = case rhs of
      [] -> [(lhs, rhs)]
      [_] -> [(lhs, rhs)]
      [_, _] -> [(lhs, rhs)]
      x : xs -> (lhs, [x, SomeNonterminal lhs']) : transform (count + 1) lhs' xs
        where
          lhs' = UnsafeMkSymbol $ lhs.text <> T.pack (show count)

-- | Eliminate ε-rules
del :: Grammar -> Grammar
del g@Grammar {productions} =
  g {productions = delProd productions}

delProd :: Productions -> Productions
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
          | otherwise ->
              (x :) <$> propEps xs
        _ -> (x :) <$> propEps xs

nullable :: Productions -> HashSet (Symbol 'Nonterminal)
nullable productions = fixpoint step $ HashMultimap.keysSet $ HashMultimap.filter null productions
  where
    step :: HashSet (Symbol 'Nonterminal) -> HashSet (Symbol 'Nonterminal)
    step old = HashMultimap.keysSet $ HashMultimap.filter (all inPrev) productions
      where
        inPrev (SomeTerminal _) = False
        inPrev (SomeNonterminal nt) = nt `HashSet.member` old

-- | Eliminate unit rules
unit :: Grammar -> Grammar
unit g@Grammar {productions} =
  g {productions = unitProd productions}

unitProd :: Productions -> Productions
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
    step old = old `HashMultimap.compose` old

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
  | x == y = x
  | otherwise = fixpoint f y
  where
    y = f x
