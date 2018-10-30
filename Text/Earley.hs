-- | Parsing all context-free grammars using Earley's algorithm.
module Text.Earley
  ( -- * Context-free grammars
    Prod, terminal, eof, (<?>), Grammar, rule
  , -- * Derived operators
    satisfy, token, namedToken, list, listLike
  , -- * Deprecated operators
    symbol, namedSymbol, word
  , -- * Parsing
    Report(..), Parser.Result(..), Parser, parser, allParses, fullParses
  , -- * Recognition
    report
  , -- * Language generation
    Generator, generator, language, upTo, exactly
  )
  where
import Text.Earley.Derived
import Text.Earley.Generator
import Text.Earley.Grammar
import Text.Earley.Parser as Parser
