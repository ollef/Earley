{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types #-}
-- | This module exposes the internals of the package: its API may change
-- independently of the PVP-compliant version number.
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

nullable :: Rule s r e t a -> Results s a
nullable r = Results $ do
  mn <- readSTRef $ ruleNullable r
  case mn of
    Just xs -> return xs
    Nothing -> do
      writeSTRef (ruleNullable r) $ Just mempty
      res <- unResults $ nullableProd $ ruleProd r
      writeSTRef (ruleNullable r) $ Just res
      return res

nullableProd :: ProdR s r e t a -> Results s a
nullableProd (Terminal _ _)    = mempty
nullableProd (NonTerminal r p) = nullable r <**> nullableProd p
nullableProd (Pure a)          = pure a
nullableProd (Alts as p)       = mconcat (map nullableProd as) <**> nullableProd p
nullableProd (Many p q)        = nullableProd $ (pure <$> p <|> pure []) <**> q
nullableProd (Named p _)       = nullableProd p

resetConts :: Rule s r e t a -> ST s ()
resetConts r = writeSTRef (ruleConts r) =<< newSTRef []

-------------------------------------------------------------------------------
-- * Delayed results
-------------------------------------------------------------------------------
newtype Results s a = Results { unResults :: ST s [a] }
  deriving Functor

instance Applicative (Results s) where
  pure  = return
  (<*>) = ap

instance Monad (Results s) where
  return = Results . pure . pure
  Results stxs >>= f = Results $ do
    xs <- stxs
    concat <$> mapM (unResults . f) xs

instance Monoid (Results s a) where
  mempty = Results $ pure []
  mappend (Results sxs) (Results sys) = Results $ (++) <$> sxs <*> sys

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
type Pos = Int

-- | An Earley state with result type @a@.
data State s r e t a where
  State :: !Pos
        -> !(ProdR s r e t a)
        -> !(a -> Results s b)
        -> !(Conts s r e t b c)
        -> State s r e t c
  Final :: Results s a -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !Pos
            -> !(a -> Results s b)
            -> !(ProdR s r e t (b -> c))
            -> !(c -> Results s d)
            -> !(Conts s r e t d e')
            -> Cont s r e t a e'
  FinalCont :: (a -> Results s c) -> Cont s r e t a c

data Conts s r e t a c = Conts
  { conts     :: !(STRef s [Cont s r e t a c])
  , contsArgs :: !(STRef s (Maybe (STRef s (Results s a))))
  }

newConts :: STRef s [Cont s r e t a c] -> ST s (Conts s r e t a c)
newConts r = Conts r <$> newSTRef Nothing

contraMapCont :: (b -> Results s a) -> Cont s r e t a c -> Cont s r e t b c
contraMapCont f (Cont cpos g p args cs) = Cont cpos (f >=> g) p args cs
contraMapCont f (FinalCont args)        = FinalCont (f >=> args)

contToState :: Results s a -> Cont s r e t a c -> State s r e t c
contToState r (Cont cpos g p args cs) = State cpos p (\f -> fmap f (r >>= g) >>= args) cs
contToState r (FinalCont args)        = Final $ r >>= args

-- | Strings of non-ambiguous continuations can be optimised by removing
--   indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont _ g (Pure f) args cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont $ \b -> fmap f (g b) >>= args) ks'
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
initialState p = State (-1) p pure <$> (newConts =<< newSTRef [FinalCont pure])

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

data ParseEnv s e i t a = ParseEnv
  { results :: ![ST s [a]]
    -- ^ Results ready to be reported (when this position has been processed)
  , next  :: ![State s a e t a]
    -- ^ States to process at the next position
  , reset :: !(ST s ())
    -- ^ Computation that resets the continuation refs of productions
  , names :: ![e]
    -- ^ Named productions encountered at this position
  , pos   :: !Pos
    -- ^ The current position in the input string
  , input :: !i
    -- ^ The input string
  }

{-# INLINE emptyParseEnv #-}
emptyParseEnv :: i -> ParseEnv s e i t a
emptyParseEnv i = ParseEnv
  { results = mempty
  , next    = mempty
  , reset   = return ()
  , names   = mempty
  , pos     = 0
  , input   = i
  }

{-# SPECIALISE parse :: [State s a e t a]
                     -> ParseEnv s e [t] t a
                     -> ST s (Result s e [t] a) #-}
-- | The internal parsing routine
parse :: ListLike i t
      => [State s a e t a] -- ^ States to process at this position
      -> ParseEnv s e i t a
      -> ST s (Result s e i a)
parse [] env@ParseEnv {results = [], next = []} = do
  reset env
  return $ Ended Report
    { position   = pos env
    , expected   = names env
    , unconsumed = input env
    }
parse [] env@ParseEnv {results = []} = do
  reset env
  parse (next env) (emptyParseEnv $ safeTail $ input env) {pos = pos env + 1}
parse [] env = do
  reset env
  return $ Parsed (concat <$> sequence (results env)) (pos env) (input env)
         $ parse [] env {results = [], reset = return ()}
parse (st:ss) env = case st of
  Final res -> parse ss env {results = unResults res : results env}
  State spos pr args scont -> case pr of
    Terminal f p -> case safeHead $ input env of
      Just t | f t -> parse ss env {next = State spos p (args . ($ t)) scont
                                         : next env}
      _            -> parse ss env
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont spos pure p args scont : ks)
      nulls <- unResults $ nullable r
      let nullStates | null nulls = mempty
                     | otherwise  = pure $ State spos p (\g -> mconcat $ map (args . g) nulls) scont
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State (pos env) (ruleProd r) pure <$> newConts rkref
        parse (st' : nullStates ++ ss)
              env {reset = resetConts r >> reset env}
      else -- The rule has already been expanded at this position.
        parse (nullStates ++ ss) env
    Pure a | spos /= pos env -> do
      let argsRef = contsArgs scont
      masref  <- readSTRef argsRef
      case masref of
        Just asref -> do -- The continuation has already been followed at this position.
          modifySTRef asref $ mappend $ args a
          parse ss env
        Nothing    -> do -- It hasn't.
          asref <- newSTRef $ args a
          writeSTRef argsRef $ Just asref
          ks  <- simplifyCont scont
          let kstates = map (contToState $ Results $ join $ unResults <$> readSTRef asref) ks
          parse (kstates ++ ss)
                env {reset = writeSTRef argsRef Nothing >> reset env}
           | otherwise -> parse ss env
    Alts as (Pure f) -> do
      let args' = args . f
          sts   = [State spos a args' scont | a <- as]
      parse (sts ++ ss) env
    Alts as p -> do
      scont' <- newConts =<< newSTRef [Cont spos pure p args scont]
      -- State is (-1) so that nullable alts are expanded correctly
      let sts = [State (-1) a pure scont' | a <- as]
      parse (sts ++ ss) env
    Many p q    -> do
      c  <- newSTRef =<< newSTRef mempty
      nr <- newSTRef Nothing
      let r   = Rule (pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)) nr c
          st' = State spos (NonTerminal r q) args scont
      parse (st' : ss) env
    Named pr' n -> parse (State spos pr' args scont : ss)
                         env {names = n : names env}

{-# INLINE parser #-}
-- | Create a parser from the given grammar.
parser :: ListLike i t
       => (forall r. Grammar r e (Prod r e t a))
       -> i
       -> ST s (Result s e i a)
parser g xs = do
  s <- initialState =<< grammar g
  parse [s] $ emptyParseEnv xs

-- | Return all parses from the result of a given parser. The result may
-- contain partial parses. The 'Int's are the position at which a result was
-- produced.
allParses :: (forall s. ST s (Result s e i a)) -> ([(a, Int)], Report e i)
allParses p = runST $ p >>= go
  where
    go :: Result s e i a -> ST s ([(a, Int)], Report e i)
    go r = case r of
      Ended rep          -> return ([], rep)
      Parsed mas cpos _ k -> do
        as <- mas
        fmap (first (zip as (repeat cpos) ++)) $ go =<< k

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
