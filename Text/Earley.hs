-- | Parsing all context-free grammars using Earley's algorithm.
module Text.Earley
  ( -- * Context-free grammars
    Prod, satisfy, (<?>), Grammar, rule
  , -- * Derived operators
    symbol, namedSymbol, word, word'
  , -- * Parsing
    Report(..), Result(..), parser, allParses, fullParses
    -- * Recognition
  , report
  )
  where
import Text.Earley.Grammar
import Text.Earley.Derived
import Text.Earley.Parser
