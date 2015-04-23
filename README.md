Earley
======

This (Text.Earley) is a library consisting of two parts:

1. Text.Earley.Grammar:
   An embedded context-free grammar (CFG) domain-specific language (DSL) with
   semantic action specification in applicative style.

   An example of a typical expression grammar working on an input tokenized
   into strings is the following:

   ```haskell
      expr :: Grammar r String (Prod r String String Expr)
      expr = mdo
        x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x2
                  <|> x2
                  <?> "sum"
        x2 <- rule $ Mul <$> x2 <* namedSymbol "*" <*> x3
                  <|> x3
                  <?> "product"
        x3 <- rule $ Var <$> (satisfy ident <?> "identifier")
                  <|> namedSymbol "(" *> x1 <* namedSymbol ")"
        return x1
        where
          ident (x:_) = isAlpha x
          ident _     = False
   ```

2. Text.Earley.Parser:
   An implementation of (a modification of) the Earley parsing algorithm.

   To invoke the parser on the above grammar, run e.g. (here using `words` as a
   stupid tokeniser):

   ```haskell
      fullParses $ parser expr $ words "a + b * ( c + d )"
      = ( [Add (Var "a") (Mul (Var "b") (Add (Var "c") (Var "d")))]
        , Report {...}
        )
   ```

   Another invocation, which shows the error reporting capabilities (giving the
   last position that the parser reached and what it expected at that point),
   is the following:

   ```haskell
      fullParses $ parser expr $ words "a +"
      = ( []
        , Report { position   = 2
                 , expected   = ["(","identifier","product"]
                 , unconsumed = []
                 }
        )
   ```

Compared to parser generators and combinator libraries
------------------------------------------------------

This library differs from the main methods that are used to write parsers in
the Haskell ecosystem:

* Compared to parser generators (YACC, Happy, etc.) it requires very little
  pre-processing of the grammar. It also allows you to stay in the host
  language for both grammar and parser, i.e. there is no use of a separate
  tool. This also means that you are free to use the abstraction facilities of
  Haskell when writing a grammar. Currently the library requires a linear
  traversal of the grammar before use, which is usually fast enough to do at
  run time, but precludes infinite grammars.

* The grammar language is similar to that of many parser combinators (Parsec,
  Attoparsec, parallel parsing processes, etc.), providing an applicative
  interface, but the parser gracefully handles all finite CFGs, including those
  with left-recursion. On the other hand, it is not monadic and does not
  support context-sensitive or infinite grammars, which are supported by many
  parser combinator libraries.

The parsing algorithm
---------------------

The parsing algorithm that this library uses is based on [Earley's parsing
algorithm](https://en.wikipedia.org/wiki/Earley_parser).  The algorithm has
been modified to produce online parse results, to give good error messages, and
to allow garbage collection of the item sets. Essentially, instead of storing a
sequence of sets of items like in the original algorithm, the modified
algorithm just stores pointers back to sets of reachable items.

The worst-case run time performance of the Earley parsing algorithm is cubic in
the length of the input, but for large classes of grammars it is linear. It
should however be noted that this library will likely be slower than most
parser generators and parser combinator libraries.

The parser implements an optimisation similar to that presented in Joop M.I.M
Leo's paper *A general context-free parsing algorithm running in linear time on
every LR(k) grammar without using lookahead*, which removes indirections in
sequences of non-ambiguous backpointers between item sets.

The implemented algorithm handles all CFGs, with one caveat: any production
that accepts the empty string (i.e. an *epsilon* production) is only expanded
*once* per position in the input string. This means that the parser does not
give an infinite number of parse results for such grammars. This is usually not
a problem.

Contact
-------

Olle Fredriksson - https://github.com/ollef
