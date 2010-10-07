import Criterion.Main
import System.Process

runner :: String -> IO ()
runner exe = do
  h <- runCommand exe
  waitForProcess h
  return ()

main :: IO ()
main = defaultMain [
        bgroup "DLA" [ bench "unopt" $ whnfIO $runner "unoptimized-haskell/dist/build/DLA/DLA params.in test.dat"
        , bench "opt1" $ whnfIO $ runner "optimized-haskell1/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt2" $ whnfIO $ runner "optimized-haskell2/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt3" $ whnfIO $ runner "optimized-haskell3/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt4" $ whnfIO $ runner "optimized-haskell4/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt5" $ whnfIO $ runner "optimized-haskell5/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt6" $ whnfIO $ runner "optimized-haskell6/dist/build/DLA/DLA params.in test.dat" 
        , bench "opt7" $ whnfIO $ runner "optimized-haskell7/dist/build/DLA/DLA params.in test.dat" ]
       ]
