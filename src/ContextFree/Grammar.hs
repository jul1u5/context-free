{-# LANGUAGE PatternSynonyms #-}
module ContextFree.Grammar
  ( -- * Grammars
    Grammar' (Grammar, terminals, productions, start, _terminals, _productions, _start),
    Grammar,
    nonterminals,

    -- * Creating grammars
    mkGrammar,
    GrammarError (..),

    -- * Symbols
    SymbolKind (..),
    Symbol(text),
    SomeSymbol(..),
    pattern SomeTerminal,
    pattern SomeNonterminal,
  )
where

import ContextFree.Grammar.Internal
