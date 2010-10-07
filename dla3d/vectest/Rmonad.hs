{-# LANGUAGE BangPatterns #-}
module Rmonad (
    DLAMonad,
    nextF,
    runRmonad,
    R(..)
) where

import System.Random.Mersenne.Pure64

data R a = R !a {-# UNPACK #-}!PureMT

newtype DLAMonad a = S { runState :: PureMT -> R a }

instance Monad DLAMonad where
    {-# INLINE return #-}
    return a = S $ \s -> R a s

    {-# INLINE (>>=) #-}
    m >>= k  = S $ \s -> case runState m s of
                                R a s' -> runState (k a) s'

    {-# INLINE (>>) #-}
    m >>  k  = S $ \s -> case runState m s of
                                R _ s' -> runState k s'

runRmonad :: DLAMonad a -> PureMT -> R a
runRmonad (S m) s = m s

nextF :: Double -> DLAMonad Double
{-# INLINE nextF #-}
nextF !up = S $ \s -> case randomDouble s of
                             (x, s') -> x `seq` R (x*up) s'
