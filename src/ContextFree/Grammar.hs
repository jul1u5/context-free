{-# LANGUAGE PatternSynonyms #-}

module ContextFree.Grammar
  ( -- * Grammars
    Grammar' (Grammar, terminals, productions, start, _terminals, _productions, _start),
    Grammar,
    nonterminals,
    prettyGrammar,

    -- * Creating grammars
    mkGrammar,
    GrammarError (..),
    prettyGrammarError,

    -- * Symbols
    SymbolKind (..),
    Symbol (text),
    SomeSymbol (..),
    pattern SomeTerminal,
    pattern SomeNonterminal,

    -- ** Symbol operations
    asTerminal,
  )
where

import ContextFree.Grammar.Internal
