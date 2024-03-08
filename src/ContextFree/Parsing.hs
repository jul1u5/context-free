{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module ContextFree.Parsing
  ( tokenize,
    tokenize',
    TokenizerError (..),
    cyk,
    CYKTable (..),
    prettyCYKTable,
  )
where

import ContextFree.Grammar
import ContextFree.Transformations (CNF)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Array.ST
import Data.HashMultimap qualified as HMM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T

type Length = Int

type Start = Int

newtype CYKTable = CYKTable {unCYKTable :: Array (Length, Start) (HashSet (Symbol 'Nonterminal))}
  deriving (Show, Eq)

newtype TokenizerError
  = UnknownTerminal Text
  deriving (Show, Eq)

tokenize :: Grammar' a -> Text -> Either TokenizerError [Symbol 'Terminal]
tokenize = tokenize' . (._terminals)

tokenize' :: HashSet (Symbol 'Terminal) -> Text -> Either TokenizerError [Symbol 'Terminal]
tokenize' terminals w =
  traverse (\t -> note (UnknownTerminal t) $ asTerminal terminals t) $
    T.words w

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- | CYK parsing algorithm
--
-- Note: grammar must be in Chomsky normal form.
-- See: 'ContextFree.Transformations.chomskyNormalForm'.
cyk :: CNF -> [Symbol 'Terminal] -> CYKTable
cyk g string = CYKTable $ runSTArray go
  where
    go :: forall s. ST s _
    go = do
      arr :: STArray s _ _ <- newArray ((1, 1), (n, n)) HS.empty

      sequence_ $ do
        s <- [1 .. n]
        pure $ do
          writeArray arr (1, s) $
            HS.fromList
              [ a
                | (a, Left t) <- HMM.toList g._productions,
                  t == w Array.! s
              ]

      sequence_ $ do
        l <- [2 .. n]
        s <- [1 .. n - l + 1]
        p <- [1 .. l - 1]
        (a, Right (b, c)) <- HMM.toList g._productions
        pure $ do
          bs <- readArray arr (p, s)
          cs <- readArray arr (l - p, s + p)
          when (b `HS.member` bs && c `HS.member` cs) $ do
            writeArray arr (l, s) . HS.insert a =<< readArray arr (l, s)

      pure arr
      where
        n = length string
        w = Array.listArray (1, n) string

-- | Show CYK table
prettyCYKTable :: CYKTable -> [Symbol 'Terminal] -> Text
prettyCYKTable (CYKTable table) w =
  T.unlines $
    reverse $
      prettyTable header rows
  where
    header = map (.text) w
    rows = do
      l <- [1 .. n]
      pure $ do
        s <- [1 .. n - l + 1]
        pure $ case HS.toList $ table Array.! (l, s) of
          [] -> "∅"
          set -> "{ " <> toText set <> " }"

    n = length w

    toText :: [Symbol a] -> Text
    toText = T.unwords . map (.text)

prettyTable :: [Text] -> [[Text]] -> [Text]
prettyTable header table =
  concat
    [ [T.intercalate "│" $ zipWith (`T.center` ' ') columnWidths header],
      [T.intercalate "┼" $ map (`T.replicate` "─") columnWidths],
      map (T.intercalate "│" . zipWith (`T.center` ' ') columnWidths . (++ [""])) table
    ]
  where
    columnWidths = map ((+ 2) . maximum . map T.length) $ transpose table
