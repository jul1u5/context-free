{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ContextFree.Grammar.Internal
import ContextFree.Grammar.Parser (PreGrammar (..), preGrammarP, asGrammar)
import ContextFree.Parsing
import ContextFree.StrongEquivalence
import ContextFree.Transformations
import Data.Array qualified as Array
import Data.Bifunctor (bimap, second)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.HashMultimap qualified as HMM
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

main :: IO ()
main = hspec $ do
  example1 <- runIO $ T.decodeUtf8 <$> BS.readFile "grammars/example1.cfg"
  example2 <- runIO $ T.decodeUtf8 <$> BS.readFile "grammars/example2.cfg"

  let postProcessedExample =
        UnsafeMkGrammar
          { terminals = HS.fromList [s "+", s "-", s "1"],
            start = s "E",
            productions =
              HMM.fromGroupedList
                [ (s "E", [[nt "E", nt "O", nt "E"], [nt "N"]]),
                  (s "O", [[t "+"], [t "-"], []]),
                  (s "N", [[t "1"], [t "1", nt "N"]])
                ]
          }

  describe "Grammar parser" $ do
    it "parses" $ do
      let parsedExample1 =
            PreGrammar
              { preTerminals = HS.fromList ["+", "-", "1"],
                preStart = "E",
                preProductions =
                  [ ("E", [["E", "O", "E"], ["N"]]),
                    ("O", [["+"], ["-"], []]),
                    ("N", [["1"], ["1", "N"]])
                  ]
              }
      let output = parse preGrammarP "" example1
      output `shouldParse` parsedExample1
      g <- tryRight $ asGrammar parsedExample1
      g `shouldBe` postProcessedExample

    it "parses with comments" $ do
      let parsedExample1 =
            PreGrammar
              { preTerminals = HS.fromList ["+", "-", "1"],
                preStart = "E",
                preProductions =
                  [ ("N", [["1", "N"], ["1"]]),
                    ("O", [["+"], [], ["-"]]),
                    ("E", [["N"], ["E", "O", "E"]])
                  ]
              }
      let output = parse preGrammarP "" example2
      output `shouldParse` parsedExample1
      g <- tryRight $ asGrammar parsedExample1
      g `shouldBe` postProcessedExample

  describe "Transformations" $ do
    describe "bin" $ do
      it "works" $ do
        (bin postProcessedExample).productions
          `shouldBe` HMM.fromGroupedList
            [ (s "E", [[nt "E", nt "E1"], [nt "N"]]),
              (s "E1", [[nt "O", nt "E"]]),
              (s "O", [[t "+"], [t "-"], []]),
              (s "N", [[t "1"], [t "1", nt "N"]])
            ]

    describe "del" $ do
      it "works on example 1" $ do
        input <-
          tryRight $
            mkGrammar
              (HS.fromList ["0"])
              [ ("S", [["0"], ["A", "B", "S"]]),
                ("A", [[], ["B", "A"]]),
                ("B", [["S"], []])
              ]
              "S"
        output <-
          tryRight $
            mkGrammar
              (HS.fromList ["0"])
              [ ("S", [["0"], ["A", "B", "S"], ["A", "S"], ["B", "S"], ["S"]]),
                ("A", [["B", "A"], ["B"], ["A"]]),
                ("B", [["S"]])
              ]
              "S"

        del input `shouldBe` output

      it "produces grammar of the right size" $ do
        let n = 10
        input <-
          tryRight $
            mkGrammar
              (HS.fromList ["0"])
              [ ("S", [concat $ replicate n ["S", "A"], []]),
                ("A", [["0"]])
              ]
              "S"

        let output = (del input).productions
        HMM.size output `shouldBe` 2 ^ n + 1

      it "works on an example from Wikipedia" $ do
        -- From https://en.wikipedia.org/wiki/Chomsky_normal_form#DEL:_Eliminate_%CE%B5-rules
        input <-
          tryRight $
            mkGrammar
              (HS.fromList ["a", "b", "c"])
              [ ("S0", [["A", "b", "B"], ["C"]]),
                ("B", [["A", "A"], ["A", "C"]]),
                ("C", [["b"], ["c"]]),
                ("A", [["a"], []])
              ]
              "S0"
        expected <-
          tryRight $
            mkGrammar
              (HS.fromList ["a", "b", "c"])
              [ ("S0", [["A", "b", "B"], ["A", "b"], ["b", "B"], ["b"], ["C"]]),
                ("B", [["A", "A"], ["A"], ["A", "C"], ["C"]]),
                ("C", [["b"], ["c"]]),
                ("A", [["a"]])
              ]
              "S0"
        del input `shouldBe` expected

    describe "unit" $ do
      it "works on example 1" $ do
        input <-
          tryRight $
            mkGrammar
              (HS.fromList ["1", "2"])
              [ ("A", [["1"], ["B"]]),
                ("B", [["2"], ["C"]]),
                ("C", [["A", "B"]])
              ]
              "A"
        expected <-
          tryRight $
            mkGrammar
              (HS.fromList ["1", "2"])
              [ ("A", [["1"], ["2"], ["A", "B"]]),
                ("B", [["2"], ["A", "B"]]),
                ("C", [["A", "B"]])
              ]
              "A"
        unit input `shouldBe` expected

  describe "Parsing" $ do
    describe "CYK" $ do
      it "works on example 1" $ do
        g <-
          tryRight $
            mkGrammar
              (HS.fromList ["0", "1", "2"])
              [ ("S", [["A", "A"]]),
                ("A", [["A", "B"], ["0"]]),
                ("B", [["1"], ["2"]])
              ]
              "S"
        g' <- tryRight $ asCNF g

        w <- tryRight $ tokenize g' "0 1 2"
        let table = cyk g' w

        let expected =
              CYKTable $
                Array.listArray ((1, 1), (3, 3)) (replicate (3 * 3) HS.empty)
                  Array.// do
                    map
                      (second $ HS.singleton . s)
                      [ ((1, 1), "A"),
                        ((1, 2), "B"),
                        ((1, 3), "B"),
                        ((2, 1), "A"),
                        ((3, 1), "A")
                      ]

        table `shouldBe` expected

  describe "StrongEquivalence" $ do
    it "produces correct relabeling" $ do
      g1 <-
        tryRight $
          mkGrammar
            (HS.fromList ["a", "b"])
            [ ("S", [["A", "B"]]),
              ("A", [["a"], ["a", "A"]]),
              ("B", [["b"], ["b", "B"]])
            ]
            "S"
      g2 <-
        tryRight $
          mkGrammar
            (HS.fromList ["a", "b"])
            [ ("A", [["B", "C"]]),
              ("B", [["a"], ["a", "B"]]),
              ("C", [["b"], ["b", "C"]])
            ]
            "A"

      let relabeling = g1 `equiv` g2
      relabeling
        `shouldBe` [ HM.fromList $
                       map
                         (bimap s s)
                         [("S", "A"), ("A", "B"), ("B", "C")]
                   ]

tryRight :: (MonadFail m, Show a) => Either a b -> m b
tryRight (Right x) = pure x
tryRight (Left x) = fail $ "tryRight: " <> show x

s :: Text -> Symbol k
s = UnsafeMkSymbol

t, nt :: Text -> SomeSymbol
t = SomeTerminal . s
nt = SomeNonterminal . s
