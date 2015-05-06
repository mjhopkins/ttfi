-- * Almost Haskell98. See CB98,hs for the genuine Haskell98 version
-- Here we use a few extensions to make the code prettier
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- * Embedding a higher-order domain-specific language (simply-typed
-- * lambda-calculus with constants) with a selectable evaluation order:
-- * Call-by-value, call-by-name, call-by-need


module CBAny where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

-- The (higher-order abstract) syntax of our DSL
type Arr repr a b = repr a -> repr b

-- This type class specifies the syntax of our EDSL
-- Its instances specify the semantics
class Symantics repr where
     int :: Int -> repr Int		-- This part is like before
     add :: repr Int -> repr Int -> repr Int
     sub :: repr Int -> repr Int -> repr Int

     app :: repr (Arr repr a b) -> repr a -> repr b

-- Only definition of lam varies across the evaluation orders
class SymLam repr where
     lam :: (repr a -> repr b) -> repr (Arr repr a b)

-- A convenient abbreviation (could've been called `bind')
let_ :: (Symantics repr, SymLam repr) => 
	repr a -> (repr a -> repr b) -> repr b
let_ x y = (lam y) `app` x

-- A sample Symantics term
t :: (Symantics repr, SymLam repr) => repr Int
t = (lam $ \x -> let_ (x `add` x)
                      $ \y -> y `add` y) `app` int 10

-- * Write a term once, evaluate many times (with different orders)

-- * //
-- * Embedding of the DSL into Haskell
-- An EDSL expression of the type a is interpreted as a Haskell value
-- of the type S l m a, where m is a Monad (the parameter of the interpretation)
-- and l is the label for the evaluation order (one of Name, Value, or Lazy).

newtype S l m a = S { unS :: m a } deriving (Monad, MonadIO)

-- * The common fragment (for all evaluation orders)
-- We use IO to print out the evaluation trace, so to observe
-- the difference in evaluation orders
instance MonadIO m => Symantics (S l m) where
  int = return
  add x y = do a <- x
	       b <- y
	       liftIO $ putStrLn "Adding"
	       return (a + b)
  sub x y = do a <- x
	       b <- y
	       liftIO $ putStrLn "Subtracting"
	       return (a - b)
  app x y = x >>= ($ y)

-- * //
-- * Call-by-name

data Name				-- The label

instance Monad m => SymLam (S Name m) where
  lam f = return f

-- Tests


runName :: S Name m a -> m a
runName x = unS x

-- The addition (x `add` x) is performed twice because y is bound
-- to a computation, and y is evaluated twice
t0SN = runName t >>= print
{-
Adding
Adding
Adding
40
-}

-- A more elaborate example
t1 :: (Symantics repr, SymLam repr) => repr Int
t1 = (lam $ \x -> let_ (x `add` x) 
                  $ \y -> lam $ \z -> 
                  z `add` (z `add` (y `add` y))) `app` (int 10 `sub` int 5) 
                                                 `app` (int 20 `sub` int 10)

t1SN = runName t1 >>= print
{-
*CB> t1SN
Subtracting
Subtracting
Subtracting
Subtracting
Adding
Subtracting
Subtracting
Adding
Adding
Adding
Adding
40
-}

-- A better example
t2 :: (Symantics repr, SymLam repr) => repr Int
t2 = (lam $ \z -> lam $ \x -> let_ (x `add` x)
                      $ \y -> y `add` y) `app` (int 100 `sub` int 10)
                                         `app` (int 5 `add` int 5)

-- The result of subtraction was not needed, and so it was not performed
-- OTH, (int 5 `add` int 5) was computed four times
t2SN = runName t2 >>= print
{-
*CB> t2SN
Adding
Adding
Adding
Adding
Adding
Adding
Adding
40
-}


-- * //
-- * Call-by-value
-- * The only difference is the interpretation of lam:
-- * before evaluating the body, _always_ evaluate the argument

data Value

    -- This is the only difference between CBN and CBV:
    -- lam first evaluates its argument, no matter what
    -- This is the definition of CBV after all
    -- lam f = return (\x -> (f . return) =<< x)
    -- or, in the pointless notation suggested by Jacques Carette
instance Monad m => SymLam (S Value m) where
    lam f = return (f . return =<<)

runValue :: S Value m a -> m a
runValue x = unS x

-- We now evaluate the previously written tests t, t1, t2
-- under the new interpretation

t0SV = runValue t >>= print
{-
*CB> t0SV
Adding
Adding
40
-}

t1SV = runValue t1 >>= print
{-
*CB> t1SV
Subtracting
Adding
Subtracting
Adding
Adding
Adding
40
-}

-- Although the result of subs-traction was not needed, it was still performed
-- OTH, (int 5 `add` int 5) was computed only once
t2SV = runValue t2 >>= print
{-
*CB> t2SV
Subtracting
Adding
Adding
Adding
40
-}

-- * //
-- * Call-by-need
-- * The only difference is the interpretation of lam:
-- * before evaluating the body, share the argument
-- * The argument is evaluated _at most_ once

share :: MonadIO m => m a -> m (m a)
share m = do
	  r <- liftIO $ newIORef (False,m)
	  let ac = do
		   (f,m) <- liftIO $ readIORef r
		   if f then m
		      else do
			   v <- m
			   liftIO $ writeIORef r (True,return v)
			   return v
	  return ac

data Lazy

instance MonadIO m => SymLam (S Lazy m) where
    -- This is the definition of CBNeed after all
    -- lam f = return (\x -> f =<< share x)
    -- Or, in the pointless notation
    lam f = return ((f =<<) . share)

-- * QUIZ: Why is this correct? Why didn't we add sharing to
-- * add and sub and app? After all, it is they which force computations.
-- * Shouldn't (add (int 1) (int 2)) be likewise enclosed in a thunk
-- * that memoizes its value when forced? Currently (add (int 1) (int 2)) 
-- * is a thunk but does no memoization. Is this correct?!

runLazy :: S Lazy m a -> m a
runLazy x = unS x

-- We now evaluate the previously written tests t, t1, t2
-- under the new interpretation

-- Here, Lazy is just as efficient as CBV
t0SL = runLazy t  >>= print
{-
*CB> t0SL
Adding
Adding
40
-}

-- Ditto
t1SL = runLazy t1 >>= print
{-
*CB> t1SL
Subtracting
Subtracting
Adding
Adding
Adding
Adding
40
-}

-- Now, Lazy is better than both CBN and CBV: subtraction was not needed,
-- and it was not performed.
-- All other expressions were needed, and evaluated once.
t2SL = runLazy t2 >>= print
{-
*CB> t2SL
Adding
Adding
Adding
40
-}

main = do
       t0SN >>= print
       t1SN >>= print
       t2SN >>= print

       t0SV >>= print
       t1SV >>= print
       t2SV >>= print

       t0SL >>= print
       t1SL >>= print
       t2SL >>= print

