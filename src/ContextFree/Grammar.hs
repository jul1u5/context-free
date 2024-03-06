module ContextFree.Grammar
  ( -- * Grammars
    Grammar (Grammar, terminals, productions, start),
    nonterminals,

    -- * Creating grammars
    MkGrammar (..),
    mkGrammar,
    GrammarError (..),

    -- * Symbols
    SymbolKind (..),
    Symbol,
    SomeSymbol(..),
    -- pattern SomeTerminal,
    -- pattern SomeNonterminal,
  )
where

import ContextFree.Grammar.Internal
