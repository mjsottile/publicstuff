--
-- quick hack to time a set of runs of the same command and compute the
-- mean and std deviation of the times.  it may be a hack, but it's better
-- than the clunky perl script I used to use ....
--
-- mjsottile@computer.org
--

import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment
import System.Process

time :: String -> IO Double
time cmd = do
  start <- getCPUTime
  h <- runCommand cmd
  waitForProcess h
  end <- getCPUTime
  let diff = (fromIntegral (end-start)) / (10^8)
  return (diff :: Double)

main = do
  [cmd,n] <- getArgs
  let cmds = replicate (read n) cmd
  times <- mapM time cmds
  let avg = (foldr (+) 0.0 times)/(read n)
  let stddev = sqrt ((foldr (+) 0.0 (map (\i -> ((i-avg)*(i-avg))) times))/((read n)-1))
  printf "Average: %0.4f sec\n" (avg :: Double)
  printf "Std Dev: %0.4f sec\n" (stddev :: Double)
  
