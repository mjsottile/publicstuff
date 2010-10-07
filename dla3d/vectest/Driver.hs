import Vec3
import Rmonad
import System.Random.Mersenne.Pure64

vecZero :: Vec3
vecZero = Vec3 0.0 0.0 0.0

stepsize :: Double
stepsize = 1.0

walker :: Vec3 -> Int -> (Double -> DLAMonad Vec3) -> DLAMonad [Vec3]
walker v 0 rv = return [v]
walker v n rv = do
  s <- rv stepsize
  slist <- (walker s (n-1) rv)
  return (v:slist)

walk :: Int -> (Double -> DLAMonad Vec3) -> DLAMonad [Vec3]
walk n rv = walker vecZero n rv

main :: IO ()
main = do
  let R s _ = runRmonad (walk 1000000 randVec) (pureMT 10)
  let R s' _ = runRmonad (walk 1000000 randVecNew) (pureMT 10)

  junk <- mapM (\(v1,v2) -> putStrLn $ show $ vecNorm $ vecSub v1 v2) (zip s s')
  return ()
