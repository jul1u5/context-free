{-# LANGUAGE OverloadedStrings #-}

module ContextFree.StrongEquivalence (Relabeling, equiv, prettyRelabeling) where

import ContextFree.Grammar
import Control.Applicative (Alternative (empty), asum)
import Control.Monad
import Control.Monad.Logic (Logic, observeAll)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashMultimap qualified as HMM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)), group)
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Arg (..))
import Data.Text (Text)
import Data.Text qualified as T

type Relabeling = HashMap (Symbol 'Nonterminal) (Symbol 'Nonterminal)

prettyRelabeling :: Relabeling -> Text
prettyRelabeling relabeling =
  T.unlines $
    map
      ( \(nt1, nt2) -> nt1.text <> " → " <> nt2.text
      )
      (HM.toList relabeling)

equiv :: Grammar -> Grammar -> [Relabeling]
equiv = \g1 g2 ->
  let relabeling = HM.singleton g1.start g2.start
      unused = g2.nonterminals `HS.difference` HS.fromList (HM.elems relabeling)
   in observeAll $ go g1 g2 g1.start relabeling unused
  where
    go ::
      Grammar ->
      Grammar ->
      Symbol 'Nonterminal ->
      Relabeling ->
      HashSet (Symbol 'Nonterminal) ->
      Logic Relabeling
    go g1 g2 initialNt relabeling unused = do
      let (outgoing1, outgoing2) =
            let nt1 = initialNt
                nt2 = relabeling HM.! nt1
             in ( g1.productions HMM.! nt1,
                  g2.productions HMM.! nt2
                )

      let outgoing1Groups = groupBy length $ HS.toList outgoing1
      let outgoing2Groups = groupBy length $ HS.toList outgoing2

      guard $ length outgoing1Groups == length outgoing2Groups

      mforM (zip outgoing1Groups outgoing2Groups) $ \((l1, group1), (l2, group2)) -> do
        guard $ l1 == l2

        mforM (NE.toList group1) $ \rule1 -> do
          rule2 <- choose $ NE.toList group2

          mforM (zip rule1 rule2) $ \case
            (SomeTerminal t1, SomeTerminal t2) -> do
              guard $ t1 == t2
              pure relabeling
            (SomeNonterminal nt1, SomeNonterminal nt2) -> do
              case relabeling HM.!? nt1 of
                Just nt1' -> do
                  guard $ nt1' == nt2
                  pure relabeling
                Nothing -> do
                  nt1' <- choose $ HS.toList unused
                  let relabeling' = HM.insert nt1 nt1' relabeling
                  let unused' = HS.delete nt1' unused
                  go g1 g2 nt1 relabeling' unused'
            _ -> empty

groupBy ::
  (Ord k) =>
  (a -> k) ->
  [a] ->
  [(k, NonEmpty a)]
groupBy f =
  map (\xs@(Arg n _ :| _) -> (n, (\(Arg _ x) -> x) <$> xs))
    . group
    . sort
    . map (\xs -> Arg (f xs) xs)

choose :: (Alternative f) => [a] -> f a
choose = asum . fmap pure

mforM :: (Monoid m, Monad f) => [a] -> (a -> f m) -> f m
mforM xs = fmap mconcat . forM xs
