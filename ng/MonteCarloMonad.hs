module MonteCarloMonad where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import System.Random

{-

simple monte carlo simulations require:

- random number generator state
- accessors to get ints, floats from PRNG
- static read only state to be passed around
- dynamic, modifiable state to be passed around
- logging of results

So:

State monad containing:
  - random number state, modifiable state
  - writer monad for logging
  - reader monad for parameters

Assume that any model being run under the monad is pure: all
IO takes place outside the monad, so no stacking of IO in here.

-}

data MonteCarloState a = MonteCarloState {
  randState :: StdGen,
  simState :: a
}

type MCMonad m a b c = StateT (MonteCarloState b) (WriterT [a] (ReaderT c m))

runMonteCarlo initstate seed params f = do 
  let gen = mkStdGen seed
  let mcstate = MonteCarloState { randState = gen, simState = initstate }
  ((a, i), l) <- runReaderT (runWriterT (runStateT f mcstate)) params
  return (a, l)
  
sampleInt :: Monad m => Int -> (MCMonad m a b c) Int
sampleInt n = do
  mcstate <- get
  let rs = randState mcstate
      (val, rs') = randomR (0,(n-1)) rs
  put ( mcstate { randState = rs' } )
  return val
  
logValue :: Monad m => a -> (MCMonad m a b c) ()
logValue val = do
  tell [val]

getSimState :: Monad m => (MCMonad m a b c) b
getSimState = do
  s <- get
  return (simState s)

updateSimState :: Monad m => b -> (MCMonad m a b c) ()
updateSimState newstate = do
  mcs <- get
  put ( mcs { simState = newstate } )

getParameters :: Monad m => (MCMonad m a b c) c
getParameters = do
  p <- ask
  return p

