{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types #-}
-- | This module exposes the internals of the package: its API may change independently of the PVP-compliant version number.
module Text.Earley.Internal where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.STRef
import Text.Earley.Grammar
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-------------------------------------------------------------------------------
-- * Concrete rules and productions
-------------------------------------------------------------------------------
-- | The concrete rule type that the parser uses
data Rule s r e t a = Rule
  { ruleProd     :: ProdR s r e t a
  , ruleNullable :: !(STRef s (Maybe [a]))
  , ruleConts    :: !(STRef s (STRef s [Cont s r e t a r]))
  }

type ProdR s r e t a = Prod (Rule s r) e t a

nullable :: Rule s r e t a -> ST s [a]
nullable r = do
  mn <- readSTRef $ ruleNullable r
  case mn of
    Just xs -> return xs
    Nothing -> do
      writeSTRef (ruleNullable r) $ Just mempty
      res <- nullableProd $ ruleProd r
      writeSTRef (ruleNullable r) $ Just res
      return res

nullableProd :: ProdR s r e t a -> ST s [a]
nullableProd (Terminal _ _)    = return mempty
nullableProd (NonTerminal r p) = do
  as <- nullable r
  concat <$> mapM (\a -> nullableProd $ fmap ($ a) p) as
nullableProd (Pure a)          = return [a]
nullableProd (Alts as p)       = (\ass fs -> fs <*> concat ass)
                              <$> mapM nullableProd as <*> nullableProd p
nullableProd (Many p q)        = do
  as <- nullableProd $ (:[]) <$> p <|> pure []
  concat <$> mapM (\a -> nullableProd $ fmap ($ a) q) as
nullableProd (Named p _)       = nullableProd p

resetConts :: Rule s r e t a -> ST s ()
resetConts r = writeSTRef (ruleConts r) =<< newSTRef []

-- | If we have something of type @f@, @'Args' s f a@ is what we need to do to
-- @f@ to produce @a@s.
type Args s f a = f -> ST s [a]

noArgs :: Args s a a
noArgs = return . pure

funArg :: (f -> a) -> Args s f a
funArg f = mapArgs f noArgs

pureArg :: x -> Args s f a -> Args s (x -> f) a
pureArg x args = args . ($ x)

impureArgs :: ST s [x] -> Args s f a -> Args s (x -> f) a
impureArgs mxs args f = fmap concat . mapM (args . f) =<< mxs

mapArgs :: (a -> b) -> Args s f a -> Args s f b
mapArgs = fmap . fmap . fmap

composeArgs :: Args s a b -> Args s b c -> Args s a c
composeArgs ab bc a = fmap concat . mapM bc =<< ab a

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
type Pos = Int

-- | An Earley state with result type @a@.
data State s r e t a where
  State :: !Pos
        -> !(ProdR s r e t f)
        -> !(Args s f b)
        -> !(Conts s r e t b a)
        -> State s r e t a
  Final :: f -> Args s f a -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !Pos
            -> !(Args s a b)
            -> !(ProdR s r e t (b -> c))
            -> !(Args s c d)
            -> !(Conts s r e t d e')
            -> Cont s r e t a e'
  FinalCont :: Args s a c -> Cont s r e t a c

data Conts s r e t a c = Conts
  { conts     :: !(STRef s [Cont s r e t a c])
  , contsArgs :: !(STRef s (Maybe (STRef s (ST s [a]))))
  }

newConts :: STRef s [Cont s r e t a c] -> ST s (Conts s r e t a c)
newConts r = Conts r <$> newSTRef Nothing

contraMapCont :: Args s b a -> Cont s r e t a c -> Cont s r e t b c
contraMapCont f (Cont pos g p args cs) = Cont pos (composeArgs f g) p args cs
contraMapCont f (FinalCont args)       = FinalCont (composeArgs f args)

contToState :: ST s [a] -> Cont s r e t a c -> State s r e t c
contToState r (Cont pos g p args cs) = 
  let mb = fmap concat . mapM g =<< r in
  State pos p (impureArgs mb args) cs
contToState r (FinalCont args)       = Final id (impureArgs r args)

-- | Strings of non-ambiguous continuations can be optimised by removing
--   indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont _ g (Pure f) args cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont $ mapArgs f g `composeArgs` args) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Interpret an abstract 'Grammar'.
grammar :: Grammar (Rule s r) e a -> ST s a
grammar g = case g of
  RuleBind p k -> do
    c  <- newSTRef =<< newSTRef mempty
    nr <- newSTRef Nothing
    grammar $ k $ NonTerminal (Rule p nr c) $ Pure id
  FixBind f k   -> do
    a <- mfix $ fmap grammar f
    grammar $ k a
  Return x      -> return x

-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
initialState p = State (-1) p noArgs <$> (newConts =<< newSTRef [FinalCont noArgs])

-------------------------------------------------------------------------------
-- * Parsing
-------------------------------------------------------------------------------
-- | A parsing report, which contains fields that are useful for presenting
-- errors to the user if a parse is deemed a failure.  Note however that we get
-- a report even when we successfully parse something.
data Report e i = Report
  { position   :: Int -- ^ The final position in the input (0-based) that the
                      -- parser reached.
  , expected   :: [e] -- ^ The named productions processed at the final
                      -- position.
  , unconsumed :: i   -- ^ The part of the input string that was not consumed,
                      -- which may be empty.
  } deriving (Eq, Ord, Read, Show)

-- | The result of a parse.
data Result s e i a
  = Ended (Report e i)
    -- ^ The parser ended.
  | Parsed (ST s [a]) Int i (ST s (Result s e i a))
    -- ^ The parser parsed a number of @a@s.  These are given as a computation,
    -- @'ST' s [a]@ that constructs the 'a's when run.  We can thus save some
    -- work by ignoring this computation if we do not care about the results.
    -- The 'Int' is the position in the input where these results were
    -- obtained, the @i@ the rest of the input, and the last component is the
    -- continuation.
  deriving Functor

{-# INLINE safeHead #-}
safeHead :: ListLike i t => i -> Maybe t
safeHead ts
  | ListLike.null ts = Nothing
  | otherwise        = Just $ ListLike.head ts

{-# INLINE safeTail #-}
safeTail :: ListLike i t => i -> i
safeTail ts
  | ListLike.null ts = ts
  | otherwise        = ListLike.tail ts

{-# SPECIALISE parse :: [State s a e t a]
                     -> [ST s [a]]
                     -> [State s a e t a]
                     -> ST s ()
                     -> [e]
                     -> Pos
                     -> [t]
                     -> ST s (Result s e [t] a) #-}
-- | The internal parsing routine
parse :: ListLike i t
      => [State s a e t a] -- ^ States to process at this position
      -> [ST s [a]]        -- ^ Results ready to be reported (when this position has been processed)
      -> [State s a e t a] -- ^ States to process at the next position
      -> ST s ()           -- ^ Computation that resets the continuation refs of productions
      -> [e]               -- ^ Named productions encountered at this position
      -> Pos               -- ^ The current position in the input string
      -> i                 -- ^ The input string
      -> ST s (Result s e i a)
parse [] [] [] reset names !pos ts = do
  reset
  return $ Ended Report {position = pos, expected = names, unconsumed = ts}
parse [] [] next reset _ !pos ts = do
  reset
  parse next [] [] (return ()) [] (pos + 1) $ safeTail ts
parse [] results next reset names !pos ts = do
  reset
  return $ Parsed (concat <$> sequence results) pos ts
         $ parse [] [] next (return ()) names pos ts
parse (st:ss) results next reset names !pos ts = case st of
  Final f args -> parse ss (args f : results) next reset names pos ts
  State spos pr args scont -> case pr of
    Terminal f p -> case safeHead ts of
      Just t | f t ->
        parse ss results (State spos p (pureArg t args) scont : next) reset names pos ts
      _            -> parse ss results next reset names pos ts
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont spos noArgs p args scont : ks)
      nulls <- nullable r
      let nullStates = [State spos p (pureArg a args) scont | a <- nulls]
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State pos (ruleProd r) noArgs <$> newConts rkref
        parse (st' : nullStates ++ ss)
              results
              next
              (resetConts r >> reset)
              names
              pos
              ts
      else -- The rule has already been expanded at this position.
        parse (nullStates ++ ss) results next reset names pos ts
    Pure a | spos /= pos -> do
      let argsRef = contsArgs scont
      masref  <- readSTRef argsRef
      case masref of
        Just asref -> do -- The continuation has already been followed at this position.
          modifySTRef asref (((++) <$> args a) <*>)
          parse ss results next reset names pos ts
        Nothing    -> do -- It hasn't.
          asref <- newSTRef $ args a
          writeSTRef argsRef $ Just asref
          ks  <- simplifyCont scont
          let kstates = map (contToState $ join $ readSTRef asref) ks
          parse (kstates ++ ss)
                results
                next
                (writeSTRef argsRef Nothing >> reset)
                names
                pos
                ts
           | otherwise -> parse ss results next reset names pos ts
    Alts as (Pure f) -> do
      let args' = funArg f `composeArgs` args
          sts   = [State spos a args' scont | a <- as]
      parse (sts ++ ss) results next reset names pos ts
    Alts as p -> do
      scont' <- newConts =<< newSTRef [Cont spos noArgs p args scont]
      -- State is (-1) so that nullable alts are expanded correctly
      let sts = [State (-1) a noArgs scont' | a <- as]
      parse (sts ++ ss) results next reset names pos ts
    Many p q    -> do
      c  <- newSTRef =<< newSTRef mempty
      nr <- newSTRef Nothing
      let r   = Rule (pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)) nr c
          st' = State spos (NonTerminal r q) args scont
      parse (st' : ss) results next reset names pos ts
    Named pr' n -> parse (State spos pr' args scont : ss) results next reset (n : names) pos ts

{-# INLINE parser #-}
-- | Create a parser from the given grammar.
parser :: ListLike i t
       => (forall r. Grammar r e (Prod r e t a))
       -> i
       -> ST s (Result s e i a)
parser g xs = do
  s <- initialState =<< grammar g
  parse [s] [] [] (return ()) [] 0 xs

-- | Return all parses from the result of a given parser. The result may
-- contain partial parses. The 'Int's are the position at which a result was
-- produced.
allParses :: (forall s. ST s (Result s e i a)) -> ([(a, Int)], Report e i)
allParses p = runST $ p >>= go
  where
    go :: Result s e i a -> ST s ([(a, Int)], Report e i)
    go r = case r of
      Ended rep          -> return ([], rep)
      Parsed mas pos _ k -> do
        as <- mas
        fmap (first (zip as (repeat pos) ++)) $ go =<< k

{-# INLINE fullParses #-}
-- | Return all parses that reached the end of the input from the result of a
--   given parser.
fullParses :: ListLike i t => (forall s. ST s (Result s e i a)) -> ([a], Report e i)
fullParses p = runST $ p >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s ([a], Report e i)
    go r = case r of
      Ended rep -> return ([], rep)
      Parsed mas _ i k
        | ListLike.null i -> do
          as <- mas
          fmap (first (as ++)) $ go =<< k
        | otherwise       -> go =<< k

{-# INLINE report #-}
-- | See e.g. how far the parser is able to parse the input string before it
-- fails.  This can be much faster than getting the parse results for highly
-- ambiguous grammars.
report :: ListLike i t => (forall s. ST s (Result s e i a)) -> Report e i
report p = runST $ p >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s (Report e i)
    go r = case r of
      Ended rep      -> return rep
      Parsed _ _ _ k -> go =<< k
