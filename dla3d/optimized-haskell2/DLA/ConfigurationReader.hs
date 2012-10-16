-- |
-- Code to read configuration files.
--
-- Author: mjsottile\@computer.org
--

module DLA.ConfigurationReader (
  readParameters
) where

import DLA.Params
import System.IO
import Data.Maybe

--
-- given a list of pairs mapping keys to values, lookup the various
-- parameters and populate the rates, genome, and simparams structures
--
extractParameters :: [(String,String)] -> DLAParams
extractParameters config = d
    where
      d = DLAParams {
            stickiness    = fromJust (lookupDouble "stickiness" config),
            death_rad     = fromJust (lookupDouble "death_rad" config),
            starting_rad  = fromJust (lookupDouble "starting_rad" config),
            min_inner_rad = fromJust (lookupDouble "min_inner_rad" config),
            inner_mult    = fromJust (lookupDouble "inner_mult" config),
            outer_mult    = fromJust (lookupDouble "outer_mult" config),
            curr_max_rad  = fromJust (lookupDouble "curr_max_rad" config),
            epsilon       = fromJust (lookupDouble "epsilon" config),
            step_size     = fromJust (lookupDouble "step_size" config),
            rng_seed      = fromJust (lookupInt "rng_seed" config),
            num_particles = fromJust (lookupInt "num_particles" config)
          }
--
-- function visible to the outside world.  passes in a string representing
-- the filename of the configuration, and passes back the params.
-- Expected to be called from within the IO monad
--
readParameters :: String -> IO DLAParams
readParameters filename = 
	do config <- readConfiguration filename
	   return $ extractParameters config

--
-- lookup helpers: float, int, char, and string versions
--

lookupDouble :: String -> [(String,String)] -> Maybe Double
lookupDouble _ [] = Nothing
lookupDouble k ((key,value):_) | (k==key)  = Just (read value)
lookupDouble k ((_,_):kvs)     | otherwise = lookupDouble k kvs

lookupInt :: String -> [(String,String)] -> Maybe Int
lookupInt _ [] = Nothing
lookupInt k ((key,value):_) | (k==key)  = Just (read value)
lookupInt k ((_,_):kvs)     | otherwise = lookupInt k kvs

lookupString :: String -> [(String,String)] -> Maybe String
lookupString _ [] = Nothing
lookupString k ((key,value):_) | (k==key)  = Just value
lookupString k ((_,_):kvs)     | otherwise = lookupString k kvs

lookupChar :: String -> [(String,String)] -> Maybe Char
lookupChar _ [] = Nothing
lookupChar k ((key,value):_) | (k==key)  = Just (head value)
lookupChar k ((_,_):kvs)     | otherwise = lookupChar k kvs

--
-- given a string, remove whitespace
--
removeWhitespace :: String -> String
removeWhitespace []                   = []
removeWhitespace (x:xs) | (x == ' ')  = removeWhitespace xs
removeWhitespace (x:xs) | (x == '\t') = removeWhitespace xs
removeWhitespace (x:xs) | otherwise   = x:(removeWhitespace xs)

--
-- split a line formatted as "KEY=VALUE", removing whitespace
--
splitLine :: String -> (String,String)
splitLine l = (front,back)
  where
    cleaned = removeWhitespace l
    front   = takeWhile (\i -> not (i == '=')) cleaned
    back    = drop 1 (dropWhile (\i -> not (i == '=')) cleaned)

--
-- read a file handle and return all of the lines in the file
--
fileToLines :: Handle -> IO [String]
fileToLines h = do eof <- hIsEOF h
                   (if eof
                    then return []
                    else do line <- hGetLine h
                            remainder <- fileToLines h
                            return $ (line:remainder))

--
-- given a filename, open the file, read the lines, and then split them
-- into key/value pairs assuming a "KEY=VALUE" format per line
--
readConfiguration :: String -> IO [(String,String)]
readConfiguration filename = do handle <- openFile filename ReadMode
                                fileLines <- fileToLines handle
                                return $ map (\i -> splitLine i) fileLines
