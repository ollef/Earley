{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types, RecursiveDo #-}
module Text.Earley.Language where
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Lazy
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.Maybe(catMaybes)
import Data.STRef.Lazy
import Text.Earley.Grammar
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-- import Debug.Trace
trace :: a -> b -> b
trace _ y = y

-------------------------------------------------------------------------------
-- * Concrete rules and productions
-------------------------------------------------------------------------------
-- | The concrete rule type that the generator uses
data Rule s r e t a = Rule
  { ruleProd  :: ProdR s r e t a
  , ruleConts :: !(STRef s (STRef s [Cont s r e t a r]))
  , ruleNulls :: !(Results s a)
  }

mkRule :: ProdR s r e t a -> ST s (Rule s r e t a)
mkRule p = mdo
  c <- newSTRef =<< newSTRef mempty
  computeNullsRef <- newSTRef $ do
    writeSTRef computeNullsRef $ return []
    ns <- unResults $ prodNulls p
    writeSTRef computeNullsRef $ return ns
    return ns
  return $ Rule (removeNulls p) c (Results $ join $ readSTRef computeNullsRef)

prodNulls :: ProdR s r e t a -> Results s a
prodNulls prod = case prod of
  Terminal {}     -> empty
  NonTerminal r p -> ruleNulls r <**> prodNulls p
  Pure a          -> pure a
  Alts as p       -> mconcat (map prodNulls as) <**> prodNulls p
  Many a p        -> prodNulls (pure [] <|> pure <$> a) <**> prodNulls p
  Named p _       -> prodNulls p

-- | Remove (some) nulls from a production
removeNulls :: ProdR s r e t a -> ProdR s r e t a
removeNulls prod = case prod of
  Terminal {}      -> prod
  NonTerminal {}   -> prod
  Pure _           -> empty
  Alts as (Pure f) -> alts (map removeNulls as) $ Pure f
  Alts {}          -> prod
  Many {}          -> prod
  Named p n        -> Named (removeNulls p) n

type ProdR s r e t a = Prod (Rule s r) e t a

resetConts :: Rule s r e t a -> ST s ()
resetConts r = writeSTRef (ruleConts r) =<< newSTRef mempty

-------------------------------------------------------------------------------
-- * Delayed results
-------------------------------------------------------------------------------
newtype Results s a = Results { unResults :: ST s [a] }
  deriving Functor

lazyResults :: ST s [a] -> ST s (Results s a)
lazyResults stas = mdo
  resultsRef <- newSTRef $ do
    as <- stas
    writeSTRef resultsRef $ return as
    return as
  return $ Results $ join $ readSTRef resultsRef

instance Applicative (Results s) where
  pure  = return
  (<*>) = ap

instance Alternative (Results s) where
  empty = Results $ pure []
  Results sxs <|> Results sys = Results $ (<|>) <$> sxs <*> sys

instance Monad (Results s) where
  return = Results . pure . pure
  Results stxs >>= f = Results $ do
    xs <- stxs
    concat <$> mapM (unResults . f) xs

instance Monoid (Results s a) where
  mempty = empty
  mappend = (<|>)

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
data BirthPos
  = Previous
  | Current
  deriving Eq

-- | An Earley state with result type @a@.
data State s r e t a where
  State :: !(ProdR s r e t a)
        -> !(a -> Results s b)
        -> !BirthPos
        -> !(Conts s r e t b c)
        -> State s r e t c
  Final :: !(Results s a) -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !(a -> Results s b)
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
contraMapCont f (Cont g p args cs) = Cont (f >=> g) p args cs
contraMapCont f (FinalCont args)   = FinalCont (f >=> args)

contToState :: BirthPos -> Results s a -> Cont s r e t a c -> State s r e t c
contToState pos r (Cont g p args cs) = State p (\f -> fmap f (r >>= g) >>= args) pos cs
contToState _   r (FinalCont args)   = Final $ r >>= args

-- | Strings of non-ambiguous continuations can be optimised by removing
-- indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont g (Pure f) args cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont $ \b -> fmap f (g b) >>= args) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
initialState p = State p pure Previous <$> (newConts =<< newSTRef [FinalCont pure])

-------------------------------------------------------------------------------
-- * Generation
-------------------------------------------------------------------------------
-- | The result of a generator.
data Result s a
  = Ended
    -- ^ The generator ended.
  | Generated (ST s [a]) Int (ST s (Result s a))
    -- ^ The generator produced a number of @a@s.  These are given as a
    -- computation, @'ST' s [a]@ that constructs the 'a's when run.  The 'Int' is
    -- the position in the input where these results were obtained, and the last
    -- component is the continuation.
  deriving Functor

{-# INLINE safeHead #-}
safeHead :: ListLike i t => i -> Maybe t
safeHead ts
  | ListLike.null ts = Nothing
  | otherwise        = Just $ ListLike.head ts

data GenerationEnv s e t a = GenerationEnv
  { results :: ![ST s [a]]
    -- ^ Results ready to be reported (when this position has been processed)
  , next    :: ![State s a e t a]
    -- ^ States to process at the next position
  , reset   :: !(ST s ())
    -- ^ Computation that resets the continuation refs of productions
  , curPos  :: !Int
    -- ^ The current position in the input string
  , tokens  :: ![t]
    -- ^ The possible tokens
  }

{-# INLINE emptyGenerationEnv #-}
emptyGenerationEnv :: [t] -> GenerationEnv s e t a
emptyGenerationEnv ts = GenerationEnv
  { results = mempty
  , next    = mempty
  , reset   = return ()
  , curPos  = 0
  , tokens  = ts
  }

-- | The internal generation routine
generate :: [State s a e t a] -- ^ States to process at this position
         -> GenerationEnv s e t a
         -> ST s (Result s a)
generate [] env@GenerationEnv {results = [], next = []} = trace "p1" $ do
  reset env
  return Ended
generate [] env@GenerationEnv {results = []} = trace "p2" $ do
  reset env
  generate (next env)
        (emptyGenerationEnv $ tokens env) {curPos = curPos env + 1}
generate [] env = trace "p3" $ do
  reset env
  return $ Generated (concat <$> sequence (results env)) (curPos env)
         $ generate [] env {results = [], reset = return ()}
generate (st:ss) env = trace "p4" $ case st of
  Final res -> trace "final" $ generate ss env {results = unResults res : results env}
  State pr args pos scont -> case pr of
    Terminal f p -> trace "terminal" $ generate ss env
      { next = [State p (\g -> Results (pure $ map (\(t, a) -> (g a, [t])) xs) >>= args) Previous scont | xs <- [catMaybes $ map (\t -> (,) t <$> f t) $ tokens env], not $ null xs]
            ++ next env 
      }
    NonTerminal r p -> trace "nonterminal" $ do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont pure p args scont : ks)
      ns    <- unResults $ ruleNulls r
      let addNullState
            | null ns = id
            | otherwise = (:)
                        $ State p (\f -> Results (pure $ map f ns) >>= args) pos scont
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State (ruleProd r) pure Current <$> newConts rkref
        generate (addNullState $ st' : ss)
              env {reset = resetConts r >> reset env}
      else -- The rule has already been expanded at this position.
        generate (addNullState ss) env
    Pure a
      -- Skip following continuations that stem from the current position; such
      -- continuations are handled separately.
      | pos == Current -> trace "pure 1" $ generate ss env
      | otherwise -> trace "pure 2" $ do
        let argsRef = contsArgs scont
        masref  <- readSTRef argsRef
        case masref of
          Just asref -> do -- The continuation has already been followed at this position.
            modifySTRef asref $ mappend $ args a
            generate ss env
          Nothing    -> do -- It hasn't.
            asref <- newSTRef $ args a
            writeSTRef argsRef $ Just asref
            ks  <- simplifyCont scont
            res <- lazyResults $ join $ unResults <$> readSTRef asref
            let kstates = map (contToState pos res) ks
            generate (kstates ++ ss)
                  env {reset = writeSTRef argsRef Nothing >> reset env}
    Alts as (Pure f) -> trace "alts 1" $ do
      let args' = args . f
          sts   = [State a args' pos scont | a <- as]
      generate (sts ++ ss) env
    Alts as p -> trace "alts 2" $ do
      scont' <- newConts =<< newSTRef [Cont pure p args scont]
      let sts = [State a pure Previous scont' | a <- as]
      generate (sts ++ ss) env
    Many p q -> mdo
      r <- mkRule $ pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)
      generate (State (NonTerminal r q) args pos scont : ss) env
    Named pr' _ -> generate (State pr' args pos scont : ss) env


{-# INLINE language #-}
-- | Generate the language for a given grammar.  The 'Int's are the lengths of
-- the strings that produce the elements.
language :: (forall r. Grammar r (Prod r e t a)) -> [t] -> [(a, Int)]
language grammar ts = runST $ generator grammar >>= ($ ts) >>= go
  where
    go :: Result s a -> ST s [(a, Int)]
    go r = case r of
      Ended -> return []
      Generated mas cpos k -> do
        as <- mas
        (zip as (repeat cpos) ++) <$> (go =<< k)

    generator :: (forall r. Grammar r (Prod r e t a))
              -> ST s ([t] -> ST s (Result s a))
    generator g = do
      let nt x = NonTerminal x $ pure id
      s <- initialState =<< runGrammar (fmap nt . mkRule) g
      return $ generate [s] . emptyGenerationEnv
