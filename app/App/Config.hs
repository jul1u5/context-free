module App.Config where

import ContextFree.Grammar
import ContextFree.Transformations

data Transformation
  = Start
  | Bin
  | Del
  | Unit
  | Term
  | CNF
  deriving (Show, Eq)

data Operation
  = Parse [Symbol 'Terminal]
  | Compare Grammar
  deriving (Show, Eq)

data Config = Config
  { grammar :: Grammar,
    transformations :: [Transformation],
    operation :: Maybe Operation
  }
  deriving (Show, Eq)

transformationToFn :: Transformation -> Grammar -> Grammar
transformationToFn = \case
  Start -> start
  Bin -> bin
  Del -> del
  Unit -> unit
  Term -> term
  CNF -> fromCNF . toCNF
