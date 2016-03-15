-- | Parsing all context-free grammars using Earley's algorithm.
module Text.Earley
  ( -- * Context-free grammars
    Prod, terminal, (<?>), Grammar, rule
  , -- * Derived operators
    satisfy, token, namedToken, list, listLike
  , -- * Deprecated operators
    symbol, namedSymbol, word
  , -- * Parsing
    Report(..), Result(..), parser, allParses, fullParses
    -- * Recognition
  , report
  )
  where
import Text.Earley.Grammar
import Text.Earley.Derived
import Text.Earley.Parser
