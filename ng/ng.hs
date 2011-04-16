--
-- implementation of a model for the Naming Game, based on
-- reading a recent paper in Phys. Rev. E.
--
-- matt@galois.com // april 2011
--

import Data.List
import System.IO
import MonteCarloMonad
import Control.Monad
import Text.Printf
import System.CPUTime

data SimParams = SimParams { 
  wordChecker :: KnowsWord,
  betaValue   :: Double 
}

--
-- specialize the monte carlo monad based on the state that we
-- use for the model state, and the type that we log
--
type NGMonad m = MCMonad m Int Int SimParams

--
-- start off by defining an individual in terms of types
--

-- words are just ints
type Word = Int

-- dictionary is a list of words
type Dictionary = [Word]

-- individual is a Dictionary
type Individual = Dictionary

--
-- next, we need a way to check if two individuals know a word
-- and update them accordingly.  this is the basic rule in which
-- if one doesn't know the word, both maintain it in their dictionaries.
-- assume A is the speaker, B is the listener.
--
knowsWord_NG :: (Individual,Individual) -> Word -> (Individual, Individual)
knowsWord_NG (a,b) w = 
  case (elem w b) of
    True  -> ([w],[w])
    False -> (a, b++[w]) 

knowsWord_HO_NG :: (Individual,Individual) -> Word -> (Individual, Individual)
knowsWord_HO_NG (a,b) w =
  case (elem w b) of
    True  -> (a,[w])
    False -> (a, b++[w])

knowsWord_SO_NG :: (Individual,Individual) -> Word -> (Individual, Individual)
knowsWord_SO_NG (a,b) w =
  case (elem w b) of
    True  -> ([w],b)
    False -> (a, b++[w])

type KnowsWord = (Individual, Individual) -> Word -> (Individual, Individual)
    
--
-- now, the word selection for one individual to tell the other.  if
-- the first doesn't have a word to say, we generate a unique one.
-- otherwise, we pick a random one and try it.
--
testWord :: Monad m => (Individual, Individual) -> KnowsWord ->
            NGMonad m (Individual, Individual)
testWord (a,b) _ | (a == []) = do
  w <- newWord
  return ([w],b++[w])
testWord (a,b) f | otherwise = do
  let n = length a
  widx <- sampleInt n
  let wval = (!!) a widx
      (a',b') = f (a,b) wval
  return (a',b')

--
-- need an effectful means to generate new words
--
newWord :: Monad m => NGMonad m Int
newWord = do
  cur <- getSimState
  updateSimState (cur+1)
  return cur
  
timestepOnePair :: Monad m => [Individual] -> NGMonad m [Individual]
timestepOnePair w = do
  let n = length w
  a <- sampleInt n
  let (aval, w') = removeAt a w
  b <- sampleInt (n-1)
  let (bval, w'') = removeAt b w'
  params <- getParameters
  (aval',bval') <- testWord (aval,bval) (wordChecker params)
  return (aval':bval':w'')
  
--
-- remove an element from a list, assuming zero indexing
-- see: http://www.haskell.org/haskellwiki/99_questions/Solutions/20
--
removeAt :: Int -> [a] -> (a,[a])
removeAt k xs = case back of
    [] -> error "removeAt: bad index"
    x:rest -> (x, front ++ rest)
  where (front,back) = splitAt k xs

--
-- create a new set of individuals
--
newIndividuals :: Int -> [Individual]
newIndividuals n = replicate n [] 

--
-- count unique words known amongst all individuals
--
data Tree = Node Word Tree Tree | Empty

-- number of unique words is number of nodes in a tree containing
-- no duplicates
numUnique p = sizeTree $ foldl (\t ps -> foldl insTree t ps) Empty p

{-# INLINE insTree #-}
insTree :: Tree -> Word -> Tree
insTree Empty          i         = Node i Empty Empty
insTree n@(Node j l r) i | i==j  = n
insTree n@(Node j l r) i | i<j   = Node j (insTree l i) r
insTree n@(Node j l r) i | i>j   = Node j l (insTree r i) 

{-# INLINE sizeTree #-}
sizeTree Empty        = 0
sizeTree (Node _ l r) = 1 + (sizeTree l) + (sizeTree r)

--
-- iterate n times
--
goNTimes :: Monad m => [Individual] -> Int -> NGMonad m ()
goNTimes p i | i < 1     = do 
  logValue (numUnique p)
  return ()
goNTimes p i | otherwise = do
  p' <- timestepOnePair p
  logValue (numUnique p')
  goNTimes p' (i-1)

--
-- IO: dump the set to a file, one value per line in ASCII
--
dumpCounts :: Show a => String -> [a] -> IO ()
dumpCounts fname vals = do
  h <- openFile fname WriteMode
  mapM (\i -> hPutStrLn h (show i)) vals
  hClose h

averager :: [[Int]] -> [Float]
averager vals =
  let n = length vals
      sums = map sum $ transpose vals
  in
    map (\i -> (fromIntegral i) / (fromIntegral n)) sums

runTrial f fname = do
  start <- getCPUTime
  let pop = replicate 200 ([]::Individual)
      seeds = [1..15]
      params = SimParams { wordChecker = f, betaValue = 1.0 }
  vals <- mapM (\i -> runMonteCarlo 0 i params $ goNTimes pop 10000) seeds
  let lists = map snd vals
  let avgs = averager lists
  dumpCounts fname avgs
  end <- getCPUTime
  let diff = (fromIntegral (end-start)) / (10^12)
  printf "%s: %0.4f sec.\n" (fname :: String) (diff :: Double)
  return ()

main :: IO ()
main = do
  runTrial knowsWord_NG "counts_ng.dat"
  runTrial knowsWord_SO_NG "counts_ho_ng.dat"
  runTrial knowsWord_HO_NG "counts_so_ng.dat"
