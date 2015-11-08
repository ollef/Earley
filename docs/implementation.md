% Earley's internals
% Olle Fredriksson
% Late 2015

This is an attempt to document the internals of the Earley library.  The
writing of this document was spurred both by a request to provide this
documentation to help contributors and by some recent discussion regarding the
readability of the implementation.

I'll first start with some background that many of you probably already know
about. Feel free to skip ahead or skim.

Background
===

Let's start by having a look at how parser combinator libraries usually work.
I'm aware that this doesn't give the full picture in terms of available
libraries for parsing --- it ignores e.g. parser generators --- but I believe
this way actually gives the best intuition for how the Earley library works.
On the surface I think that the Earley library is more similar to combinator
libraries than to parser generators because of the (Applicative, embedded in a
powerful host language) interface that it provides. However, the
*functionality* that it provides may more accurately be compared to parser
generator libraries.

Parser combinators
-------

I know of three ways that parser combinator libraries handle alternatives:

* *Greedy*, or LL(1): Commit to an alternative the instant it has consumed a
  symbol.  Always start with the leftmost alternative. This is classical
  recursive descent.
* *Backtracking*: Go back and try the other alternatives if an
  alternative fails (i.e. depth-first).
* *Parallel*: Try all alternatives in parallel (i.e. breadth-first)

Some examples of libraries in these categories are

* [Parsec](https://hackage.haskell.org/package/parsec), which is greedy by default, but backtracks from wherever the `try`
  combinator is used,
* [Attoparsec](https://hackage.haskell.org/package/attoparsec), which is backtracking, and
* [Parsek](https://hackage.haskell.org/package/parsek) (a.k.a. parallel parsing processes), which is parallel.

The following is a small expression language given in BNF:

```
ident ::= 'x' | 'y'
expr  ::= ident | ident '+' expr
```

An example string in this language is `"x+y"`.  Let's investigate how the three
different kinds of alternative handling cope with this input string.

* Greedy:

    This starts by choosing the first `expr` branch and parsing an `'x'`,
    i.e.  the derivation
    ```
    expr ---->{1} ident
         ---->{1} 'x'
         -->{'x'} EMPTY
         -------> FAIL
    ```
    I use the following kinds of transitions in the derivation:
    - ` a  rest ---->{i} a_i rest`: expand the leftmost non-terminal's `i`th production.
    - `'x' rest -->{'x'} rest`: consume token 'x' from the input.

    Since the parser has managed to parse a symbol at this point, it has
    committed to the first `expr` branch and makes no further progress. Here I
    use `EMPTY` to denote the empty production (sometimes also called epsilon).
* Backtracking:

    We take the transition `FAIL ->{back} y` to mean backtracking to some
    previous state. I'll use indentation to indicate what the state is; this is
    slightly informal (in an implementation we would have to save some piece of
    state), but hopefully clear enough.
    ```
    expr ---->{1} ident
         ---->{1} 'x'
         ------> FAIL
         ->{back}
    expr ---->{2} ident '+' expr
         ---->{1} 'x' '+' expr 
         -->{'x'} '+' expr
         -->{'+'} expr
         ---->{1} ident ---->{1} 'x'
                        -------> FAIL
                        ->{back}
                  ident ---->{2} 'y'
                        -->{'y'} EMPTY
                        -->{EOF} SUCCESS
    ```
* Parallel:

    Here we use the syntax `a rest ---->{*} (a_1 rest | ... | a_k rest)`
    for expanding all the productions of a non-terminal.
    ```
    expr ---->{*} ident | ident '+' expr
         ---->{*} 'x' | 'y' | 'x' '+' expr | 'y' '+' expr
         -->{'x'} EMPTY | '+' expr
         -->{'+'} expr
         ---->{*} ident | ident '+' expr
         ---->{*} 'x' | 'y' | 'x' '+' expr | 'y' '+' expr
         -->{'y'} EMPTY | '+' expr
         -->{EOF} SUCCESS
    ```

We saw that greedy parsing did not succeed like the others. So what is it good
for? First of all, we can rewrite the grammar and make it work. For example, as
follows:

```
ident ::= 'x' | 'y'
tail ::= '+' ident tail | EMPTY
expr  ::= ident tail
```

For the example string, `"x+y"`, we get the following derivation:

```
expr ---->{1} ident tail
     ---->{1} 'x' tail
     -->{'x'} tail
     ---->{1} '+' ident tail
     -->{'+'} ident tail ---->{1} 'x' tail
                         -------> FAIL
                         ->{back}
              ident tail ---->{2} 'y' tail
                         -->{'y'} tail ---->{1} '+' ident tail
                                       -------> FAIL
                                       ->{back}
                                  tail ---->{2} EMPTY
                                       -------> SUCCESS
```

Note that this parser also backtracks, but only if it hasn't consumed
any input in the branch it backtracks from. The nice property of a greedy
parser, compared to a backtracking parser, is that it never backtracks *in the
input string*.  This means that it is potentially more efficient than a
backtracking parser, because it does not have to keep references to old
positions in the input string when keeping backtracking states. I say
potentially faster, because Attoparsec, probably the fastest parser combinator
library for Haskell currently in existence, does full backtracking seemingly
without suffering from any performance penalty.

Both backtracking and parallel parsing have the same power in terms of the
languages that they recognise.  If we are only interested in recognition and
our language is not LL(1) or not easily factorable into LL(1), backtracking
parsing is a practical choice, since it often does less work than a
parallel parser. Note, however, that there are cases where a backtracking
parser has to do as much work as a parallel parser. If we are interested in
getting *all* the ways a given string is parsable, parallel parsing is
a natural choice.

The limitations of parser combinators
---------

* Language:

    Backtracking and parallel parsers are guaranteed to terminate only for any
    LL(k) language. For example, they do not handle left-recursive grammars,
    i.e. grammars where we have derivations of the form `p -->+ p rest`.

    As an example, they cannot handle the following grammar, because `expr` is
    left-recursive:

    ```
    ident ::= 'x' | 'y'
    expr  ::= ident | expr '+' ident
    ```

    In practice, we can often perform *left-recursion removal*. The cost of
    this is that the intended meaning of the grammar is sometimes obscured.

* Worst-case running time:

    There are many grammars for which parsing is exponential.

    Consider:

    ```
    x ::= '(' x x ')' | EMPTY
    ```

    Let's see what happens in trying to parse the string `"(())"` in parallel:

    ```
    x ---->{*} '(' x x ')' | EMPTY
      -->{'('} x x ')'
      ---->{*} '(' x x ')' x ')' | x ')'
      ---->{*} '(' x x ')' x ')' | '(' x x ')' ')' | ')'
      -->{'('} x x ')' x ')' | x x ')' ')'
      ---->{*} '(' x x ')' x x ')' x ')' | x ')' x ')' | x x ')' ')'
      ---->{*} '(' x x ')' x x ')' x ')' | '(' x x ')' ')' x ')' | ')' x ')' | x x ')' ')'
      ---->{*} '(' x x ')' x x ')' x ')' | '(' x x ')' ')' x ')' | ')' x ')' | '(' x x ')' x ')' ')' | x ')' ')'
      ---->{*} '(' x x ')' x x ')' x ')' | '(' x x ')' ')' x ')' | ')' x ')' | '(' x x ')' x ')' ')' | '(' x x ')' ')' ')' | ')' ')'
      -->{')'} x ')' | ')'
      ---->{*} '(' x x ')' ')' | ')' | ')'
      -->{')'} EMPTY | EMPTY
      -------> SUCCESS
    ```
    We can at least see that the number of parallel productions that we are
    processing blows up quite quickly.
    I find it easiest to see this problem in the parallel strategy, but note
    that backtracking parsing has to do the same amount of work as the parallel
    parser for certain inputs.

    This should also have convinced you that quite a lot of the work is duplicated.


Both of these problems stem from the way that parser combinator grammars are
written.
Most recursive grammars written using parser combinators are not finite, even though
they have a finite representation, and *there is no way to detect this*.

This is similar to how we can't detect (without using dirty, unsafe tricks)
that we will never reach the end of the list defined by `ones = 1 : ones`.

In this light, let's look at the left-recursive grammar again:

```
expr ::= ident | expr '+' ident
```

As a parser combinator grammar, this is equivalent to the following, which
elucidates the left-recursion problem:

```
expr ::= ident | (ident | (ident | (ident | ...) '+' ident) '+' ident) '+' ident
```

Since parser combinators have no way of distinguishing between non-terminals
and their productions, they also have *no way* of sharing work between
invocations of the same non-terminal, because they cannot detect that this
has happened in the first place.


How not to repeat yourself yourself
-----

I know of two situations where there is potential to share work.

The first is when we encounter the same non-terminal at the same position.
As an example, there is no need to re-do the work of `x` in both branches
of `a` in the following grammar.

```
a ::= x rest1 | x rest2
```

A better idea would be to just parse `x`
once and carry on with `rest1 | rest2`.
Pictorially, we want the following branching structure:
```
       rest1
      /
 a---x
      \
       rest2
```
We do not want the following, which we have in parallel parsing:
```
   x--rest1
  /
 a
  \
   x--rest2
```

A more general example is whenever we are parsing two unrelated branches at the same position, such as:
```
a ::= start1 x rest1
b ::= start2 x rest2
```
If we are parsing `a` and `b` at the same time we also want to share the work
of the non-terminal `x` whenever it's encountered at the same position:
```
a---start1   --rest1
          \ /
           x
          / \
b---start2   --rest2
```
What we don't want is to keep the two branches independent:
```
a---start1---x---rest1

b---start2---x---rest2
```

It turns out that if we never re-do the work of a non-terminal, we can
automatically gain support for left-recursive grammars, since the main problem
there is the infinite expansion of recursive non-terminals.

The second way we can share work is perhaps less obvious. Here I will use an
extended BNF syntax where productions can have alternatives anywhere in the
tree. As an example

```
a ::= (x | y) rest
```
means that `a` accepts either an `x` or a `y`, followed by `rest`.
One (bad) way to handle `(x | y) rest` is to desugar it as follows:

```
(x | y) rest = x rest | y rest
```

Now let's say that in parsing `a`, both branches `x` and `y` are successful
and finish at the same time.  Unless `rest` is a non-terminal and falls
into the work-sharing situation above, we will be parsing two instances of
the same thing. It would be more efficient if we could share the work of
`rest`.

Pictorially, we want the following branching structure:
```
  x--
 /   \
a     rest
 \   /
  y--

```
We do not want the following, which we sometimes have in combinator libraries.
```
  x---rest
 /
a
 \
  y---rest

```

This second point also applies if instead of `x | y` we have a non-terminal
that expands to multiple branches.


The Earley library and the essence of Earley
---

There are already good presentations of Earley's algorithm available,
so I will not repeat the full definition here. To give some context, here's the
core of the algorithm, adapted from
[Wikipedia](https://en.wikipedia.org/wiki/Earley_parser):

-----------

The state set at input position `k` is called `S(k)`. The parser is seeded with
`S(0)` consisting of only the top-level rule. The parser then repeatedly executes
three operations: prediction, scanning, and completion.

* _Prediction_: For every state in `S(k)` of the form `(X -> a . Y b, j)` (where `j`
    is the origin position as above), add `(Y -> . y, k)` to `S(k)` for every production
    in the grammar with `Y` on the left-hand side `(Y -> y)`.
* _Scanning_: If `a` is the next symbol in the input stream, for every state in
    `S(k)` of the form `(X -> a . 'a' b, j)`, add `(X -> a 'a' . b, j)` to `S(k+1)`.
* _Completion_: For every state in `S(k)` of the form `(X -> y ., j)`, find
    states in `S(j)` of the form `(Y -> a . X b, i)` and add
    `(Y -> a X . b, i)` to `S(k)`.

It is important to note that duplicate states are not added to the state set,
only new ones. These three operations are repeated until no new states can be
added to the set. The set is generally implemented as a queue of states to
process, with the operation to be performed depending on what kind of state it
is.

-----------

The note that duplicate states are not added is very important. It means that:

1. If the _prediction_ step expands the same non-terminal
    multiple times at the same position, no more than one copy of each of the
    non-terminal's productions are added to the current state set.

2. If the _completion_ step completes a non-terminal multiple
    times at the same position, no more than one copy of each of the
    *completions* from the earlier state that it refers to are added to the
    current state set.

These points are the essence of Earley parsing, and mean that we are work-sharing
precisely as outlined above.

State sets
---

The state set does not have to be a set, as long as we follow the two points
above (note that they make no use of set-specific properties, but only that we
do not expand the same thing more than once per position).  We can use whatever
representation of state collections that we find appropriate as long as we do
that. In the Earley library, productions can contain functions, e.g.
of type `token -> Bool` for matching input tokens with arbitrary predicates,
which means that they are not in general comparable (they are not in the `Ord`
typeclass). Since this means that we cannot use `Set` or similar containers, we
instead use lists of states.

Additionally, we do not have to keep states from earlier positions around,
as long as we have enough information to perform the completion step.

The Earley library keeps only two lists of Earley states: One for the current
position and one for the next position, and follows the two points above by
machinery that will now be described.

The representation of states is different from the classical presentations
of Earley's algorithm.
We represent a state as a production and a *continuation pointer*. We keep only
the part of the production that is left to parse, i.e. we drop everything before
"the dot" in Earley's states. We have a special `Final` state for
when we are done.
The continuation pointer is a mutable reference to a list of continuations,
which correspond to possible completions in the original algorithm.
For the moment we can think of a continuation as being the same kind
of thing as a state, though technically they differ slightly because
when we are also dealing with *parse results*, continuations accept
such a parse result before they can meaningfully be followed.

Let's have a look at an example.
A state (a production and a mutable reference to a list of states) might
look a little bit like this:

```
  +------------------+
  |                  |
  v                  |
cont1       (prod1, ptr)
cont2

```

I said that continuations are pretty much also states, so there might be
more continuation pointers going left until we reach the special `Final`
state.

The parsing operations
---

When parsing we have a list of such states for the current position,
and a list for the next position. When all the states for the current
position have been processed, we advance the position in the input string and
make the next-states the current, and use the empty list for the new
next-states.

The *scanning* step is done just like in Earley's original algorithm;
when processing `(prod1, ptr)`,
if `prod1 = 'x' prod1'` and the current position in the input is an `'x'`,
then we add `(prod1', ptr)` to the list for the next position.

The *prediction* step, i.e. the expansion of a non-terminal, is a bit
more peculiar. First we need to look at how a non-terminal is represented.

We are using the generalised representation of productions from above where
alternatives can occur anywhere in the tree (which also means that productions
are in the standard Haskell `Alternative` typeclass), so we just need to associate
a production (and not e.g. a list of productions) with every non-terminal.

Every non-terminal is also associated with a mutable reference to a mutable
reference (no, that is not a typo) to a list of continuations. So a non-terminal
might look a bit like this:

```
      cont1
      cont2
      cont3
       ^
   +---+
   |
   |
innerPtr<--+
           |
           |
(prod, outerPtr)
```

The outermost reference (`outerPtr` above) is made sure to point to a fresh
`innerPtr` that points to the empty list every time we advance the position in
the input string.

Since both the production and the pointer depend on the type of the
non-terminal, we cannot easily store this information in e.g. a `Map`
because it would have to contain elements of different types (i.e. be
*heterogeneous*).

The solution to this typed associated-information problem, perhaps obvious to
some, is to store the associated information *in* the non-terminals.  The
representation of non-terminals in the library is thus exactly this associated
information: a production and the reference reference.

Now we are ready to do *prediction*.
Let's say our current-list is `[(x prod1, ptr1), (x prod2, ptr2)]`,
where the non-terminal `x = (xProd, xPtr)`, and our next-list is `[]`.
This means that we have two states that begin with the same non-terminal,
but end with different productions (`prod1` and `prod2`), and that additionally
have continuations from before (`ptr1` and `ptr2`).

Assuming that it hasn't been expanded before at this position, `x` looks
something like this:

```
      [ ]
       ^
   +---+
   |
   |
innerXPtr<--+
            |
            |
  (xProd, xPtr)
```

The *prediction* step takes place when we process our first state,
`(x prod1, ptr1)`.
We then add the continuation `(prod1, ptr1)` to the inner list that `x` refers
to, and add the state `(xProd, innerXPtr)` to the current-list.

When we process the second state `(x prod2, ptr2)`, we can detect that `x` has
already been expanded at this position, because the continuation list is
non-empty. When this happens we add `(prod2, ptr2)` to the inner list that `x`
refers to, but we *do not* add any new state to the current-list --- because
`xProd` is already there. At this point we have the following pointer
structure:

```
 (prod1, ptr1)
 (prod2, ptr2)         current-list
       ^            (xProd, innerXPtr)
       |                        |
   +---+------------------------+
   |
innerXPtr<--+
            |
            |
  (xProd, xPtr)
```

We can see in the above that we are sharing the work of parsing `x`, and simply
keep track of the two continuations for when that has been done.
Here we can also see why we need a double reference from non-terminals. We
clear every non-terminal when we advance the position, but we still
want to keep the pointers between any states and continuations. Clearing
in the above picture would mean to mutate `xPtr` to point to a fresh inner
reference that points to the empty list. However, the `innerXPtr` from any
states in the next-list stay intact.

Now we come to *completion*, which is what happens when we process a state
of the form `(EMPTY, ptr)`. Then we simply look up the continuations
by following `ptr` and add those to the current-list.
Pretending that `xProd` was `EMPTY` In the above picture, we would add
`(prod1, ptr1)` and `(prod2, ptr2)` to the current-list.

Since we do not tie alternatives to non-terminals, we have an additional
operation which happens when we encounter alternatives. In the original
Earley algorithm this is baked into the *prediction* step, and in our
formulation of the algorithm this operation is basically the same
as *prediction*.

When processing a state `((alt1 | alt2) prod, ptr)` we create
a new continuation pointer, `newptr` that points to `(prod, ptr)`,
and continue with the states `(alt1, newptr)` and `(alt2, newptr)`.
With this operation in place we have to be more careful when we do *completion*.
Just like we made sure that a non-terminal is only expanded once per position
in the input we have to make sure that completion of a continuation is only
done once per position. For now we can think of this as pairing a mutable
boolean with each list of continuations that we point to, though this is
slighly more complicated when we also have to deal with parse results.

TODO simplifyCont/Leo's optimisation

Recognition vs. parsing
---

A recogniser for a language is a program that decides if a given input string
is in the language. Parsing additionally creates a parse tree.  So far we have
only really discussed recognition.

In Haskell it is convenient to give an `Applicative` interface to our parsers,
which allows us to attach semantic actions to parsers without having to
construct and interpret an intermediate parse tree.

TODO explain delayed results and how they cope with infinitely and exponentially
many result
