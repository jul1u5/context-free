{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ContextFree.Grammar.Internal (Grammar (..), MkGrammar (..), SSymbolKind (..), SomeSymbol (..), Symbol (UnsafeMkSymbol), mkGrammar)
import ContextFree.Grammar.Parser (grammarP)
import ContextFree.Transformations
import Data.ByteString qualified as BS
import Data.HashMultimap qualified as HashMultimap
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  example1 <- runIO $ T.decodeUtf8 <$> BS.readFile "grammars/example1.cfg"
  example2 <- runIO $ T.decodeUtf8 <$> BS.readFile "grammars/example2.cfg"

  let parsedExample1 =
        UnsafeMkGrammar
          { terminals = HashSet.fromList [s "+", s "-", s "1"],
            start = s "E",
            productions =
              HashMultimap.fromGroupedList
                [ (s "E", [[nt "E", nt "O", nt "E"], [nt "N"]]),
                  (s "O", [[t "+"], [t "-"], []]),
                  (s "N", [[t "1"], [t "1", nt "N"]])
                ]
          }

  describe "Grammar parser" $ do
    it "parses" $ do
      parse grammarP "" example1 `shouldParse` parsedExample1

    it "parses with comments" $ do
      parse grammarP "" example2 `shouldParse` parsedExample1

  describe "Transformations" $ do
    describe "bin" $ do
      it "works" $ do
        binProd parsedExample1.productions
          `shouldBe` HashMultimap.fromGroupedList
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
              MkGrammar
                { terminals = HashSet.fromList ["0"],
                  start = "S",
                  productions =
                    [ ("S", [["0"], ["A", "B", "S"]]),
                      ("A", [[], ["B", "A"]]),
                      ("B", [["S"], []])
                    ]
                }
        output <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["0"],
                  start = "S",
                  productions =
                    [ ("S", [["0"], ["A", "B", "S"], ["A", "S"], ["B", "S"], ["S"]]),
                      ("A", [["B", "A"], ["B"], ["A"]]),
                      ("B", [["S"]])
                    ]
                }

        delProd input.productions `shouldBe` output.productions

      it "produces grammar of the right size" $ do
        let n = 10
        input <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["0"],
                  start = "S",
                  productions =
                    [ ("S", [concat $ replicate n ["S", "A"], []]),
                      ("A", [["0"]])
                    ]
                }

        let output = delProd input.productions
        HashMultimap.size output `shouldBe` 2 ^ n + 1

      it "works on an example from Wikipedia" $ do
        -- From https://en.wikipedia.org/wiki/Chomsky_normal_form#DEL:_Eliminate_%CE%B5-rules
        input <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["a", "b", "c"],
                  start = "S0",
                  productions =
                    [ ("S0", [["A", "b", "B"], ["C"]]),
                      ("B", [["A", "A"], ["A", "C"]]),
                      ("C", [["b"], ["c"]]),
                      ("A", [["a"], []])
                    ]
                }
        expected <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["a", "b", "c"],
                  start = "S0",
                  productions =
                    [ ("S0", [["A", "b", "B"], ["A", "b"], ["b", "B"], ["b"], ["C"]]),
                      ("B", [["A", "A"], ["A"], ["A", "C"], ["C"]]),
                      ("C", [["b"], ["c"]]),
                      ("A", [["a"]])
                    ]
                }
        delProd input.productions `shouldBe` expected.productions

    describe "unit" $ do
      it "works on example 1" $ do
        input <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["1", "2"],
                  start = "A",
                  productions =
                    [ ("A", [["1"], ["B"]]),
                      ("B", [["2"], ["C"]]),
                      ("C", [["A", "B"]])
                    ]
                }
        expected <-
          tryRight $
            mkGrammar
              MkGrammar
                { terminals = HashSet.fromList ["1", "2"],
                  start = "A",
                  productions =
                    [ ("A", [["1"], ["2"], ["A", "B"]]),
                      ("B", [["2"], ["A", "B"]]),
                      ("C", [["A", "B"]])
                    ]
                }
        unitProd input.productions `shouldBe` expected.productions

tryRight :: (MonadFail m, Show a) => Either a b -> m b
tryRight (Right x) = pure x
tryRight (Left x) = fail $ "tryRight: " <> show x

s :: Text -> Symbol k
s = UnsafeMkSymbol

t, nt :: Text -> SomeSymbol
t = SomeSymbol STerminal . s
nt = SomeSymbol SNonterminal . s
