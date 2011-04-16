--
-- implementation of a model for the Naming Game, based on
-- reading a recent paper in Phys. Rev. E.
--
-- matt@galois.com // april 2011
--

import Data.List
import Data.IORef
import System.Random
import Debug.Trace
import System.IO

--
-- we're going to need some mutable state.  so let's make a data
-- structure for the state based on some iorefs.
--
data NGState = NGState {
  randState :: IORef StdGen,
  wordState :: IORef Int
}

newNGState :: IO NGState
newNGState = do
  r <- newIORef (mkStdGen 1)
  w <- newIORef 0
  return $ NGState { randState = r, wordState = w }

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
    
--
-- now, the word selection for one individual to tell the other.  if
-- the first doesn't have a word to say, we generate a unique one.
-- otherwise, we pick a random one and try it.
--
testWord :: (Individual, Individual) -> NGState -> IO (Individual, Individual)
testWord (a,b) s | (a == []) = do
  w <- newWord s
  return ([w],b++[w])
testWord (a,b) s | otherwise = do
  let n = length a
  widx <- getInt s n
  let wval = (!!) a widx
      (a',b') = knowsWord_NG (a,b) wval
  return (a',b')

--
-- random number from 0 to n-1
--
getInt :: NGState -> Int -> IO Int
getInt s n = do
  let randGen = randState s
  gen <- readIORef randGen
  let (val, gen') = randomR (0,(n-1)) gen
  writeIORef randGen gen'
  return val

--
-- need an effectful means to generate new words
--
newWord :: NGState -> IO Int
newWord w = do
  let wordGenerator = wordState w
  cur <- readIORef wordGenerator
  writeIORef wordGenerator (cur+1)
  return cur
  
--
-- now, given a set of individuals, pick them in pairs to update the
-- state of the world
--
timestep :: [Individual] -> NGState -> IO [Individual]
timestep w s = do
  let n = length w
  pairs <- pairify [0..(n-1)] s
  w' <- mapM (\(a,b) -> testWord (w !! a, w !! b) s) pairs
  return $ foldl1 (++) $ map (\(a,b) -> [a,b]) w'
  
timestepOnePair :: [Individual] -> NGState -> IO [Individual]
timestepOnePair w s = do
  let n = length w
  a <- getInt s n
  let (aval, w') = removeAt a w
  b <- getInt s (n-1)
  let (bval, w'') = removeAt b w'
  (aval',bval') <- testWord (aval,bval) s
  return (aval':bval':w'')
  
--
-- randomly pair up elements from a sequence
--
pairify :: [Int] -> NGState -> IO [(Int,Int)]
pairify []      s = return []
pairify indices s = do
  let n = length indices
  a <- getInt s n
  let (aval, i') = removeAt a indices
  b <- getInt s (n-1)
  let (bval, i'') = removeAt b i'        
  remainder <- pairify i'' s
  return $ [(aval,bval)] ++ remainder

--
-- remove an element from a list, assuming zero indexing
-- see: http://www.haskell.org/haskellwiki/99_questions/Solutions/20
--
removeAt :: Int -> [a] -> (a,[a])
removeAt k xs = case back of
    [] -> error "bad index"
    x:rest -> (x, front ++ rest)
  where (front,back) = splitAt k xs

--
-- create a new set of individuals
--
newIndividuals :: Int -> [Individual]
newIndividuals n = replicate n [] 


{-
numUnique :: [Individual] -> Int
numUnique p =
  let removeDups []     accum    = accum
      removeDups (x:xs) []       = removeDups xs [x]
      removeDups (x:xs) a@(y:ys) = if (x==y) then removeDups xs a
                                             else removeDups xs (x:a)
      flattened = foldl1 (++) p
      fsort = sort flattened
      fsnodup = removeDups fsort []
  in
    length fsnodup
-}

--numUnique :: [Individual] -> Int
--numUnique p = length $ nub $ foldl1 (++) p
numUnique :: [Individual] -> Int
numUnique p = length $ foldl1 union p



goNTimes :: [Individual] -> NGState -> Int -> IO [Int]
goNTimes p _ i | i < 1     = do return [numUnique p]
goNTimes p s i | otherwise = do
  p' <- timestepOnePair p s
  let nu = numUnique p'
  pfin <- goNTimes p' s (i-1)
  return (nu:pfin)

dumpCounts :: String -> [Int] -> IO ()
dumpCounts fname vals = do
  h <- openFile fname WriteMode
  mapM (\i -> hPutStrLn h (show i)) vals
  hClose h

--
-- main
--
main :: IO ()
main = do
  st <- newNGState
  let pop = replicate 200 ([]::Individual)
  counts <- goNTimes pop st 10000
  dumpCounts "counts.dat" counts
