-- |
--  Monad based on state for passing random number state around for DLA.
--  The choice of Mersenne.Pure64 was for performance, and the pure version
--  will play nicely with threading.
-- 
--  Author: mjsottile\@computer.org
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DLA.Rmonad (
    DLAMonad,
    nextF,
    runRmonad
) where

import System.Random.Mersenne.Pure64
import Control.Monad.State.Strict

newtype Rmonad s a = S (State s a)
    deriving (Monad)

-- | The DLAMonad is just a specific instance of the State monad where the
--   state is just the PureMT PRNG state.
type DLAMonad a = Rmonad PureMT a

-- | Generate a random number as a Double between 0.0 and the given upper
--   bound.
nextF :: Double -- ^ Upper bound.
      -> Rmonad PureMT Double
nextF up = S $ do st <- get
                  let (x,st') = randomDouble st
                  put st'
                  return (x*up)

-- | Run function for the Rmonad.
runRmonad :: Rmonad PureMT a -> PureMT -> (a, PureMT)
runRmonad (S m) s = runState m s
