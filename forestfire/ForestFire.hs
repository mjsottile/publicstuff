module ForestFire (
  Size(..),
  World(..),
  CellState(..),
  idx,
  initialize,
  oneStep
) where

import Control.Monad (replicateM)
import Data.IORef
import qualified Data.Vector as U
import System.Random.Mersenne.Pure64

-- size of the world
data Size = Size Int Int

-- cells can be in one of three states
data CellState = Empty | Tree | Burning
  deriving Eq

-- the world is a vector of cells and a size
data World = World { wData :: U.Vector CellState, wSize :: Size }

-- turn 2D coordinates from region of given size into 1D offset
idx :: Int -> Int -> Size -> Int
idx x y (Size w h) = ((y `mod` h)*w)+(x `mod` w)

-- turn 1D offset into coordinates within region of given size
unidx :: Int -> Size -> (Int,Int)
unidx i (Size w h) = (i `mod` w, i `div` w)

-- initialize world to all empty
initialize :: Size -> World
initialize s@(Size w h) =
  World { wData = U.generate (w*h) (\i -> Empty),
          wSize = s }

-- given a coordinate, find linearized indices of neighbors
neighbors :: Int -> Size -> [Int]
neighbors i s =
  [idx (x+1) y s, idx (x+1) (y-1) s, idx (x+1) (y+1) s,
   idx (x-1) y s, idx (x-1) (y-1) s, idx (x-1) (y+1) s,
   idx x (y-1) s, idx x (y+1) s]
  where
    (x,y) = unidx i s

localRule :: CellState -> Int -> Bool -> Bool -> CellState
localRule s ncount ptest ftest =
  case s of
    Burning -> Empty
    Empty   -> if ptest then Tree else Empty
    Tree    -> if ftest then Burning 
                        else if (ncount > 0) then Burning 
                                             else Tree

nextF :: (IORef PureMT) -> IO Double
nextF st = 
  do rst <- readIORef st
     let (v,rst') = randomDouble rst
     writeIORef st rst'
     return v

oneStep :: World -> (IORef PureMT) -> Double -> Double -> IO World
oneStep w st p f = 
  do
    let x = wData w
    let s@(Size sw sh) = wSize w
    pval <- replicateM (sw*sh) (nextF st)
    fval <- replicateM (sw*sh) (nextF st)
    let xpf = U.zip3 x (U.fromList pval) (U.fromList fval)
    let w' = World { wData = U.imap (ruleContext w p f) xpf, wSize = s }
    return w'

-- this function calls the local rule in the context of its neighborhood.
-- this is separate from localRule to decouple the notion of neighborhood
-- from the single-cell centric definition of the rule.
ruleContext w p f i (v,pv,fv) = localRule v ncount (pv > p) (fv > f)
  where
    ns = neighbors i (wSize w)
    ncount = foldl (+) 
                   0 
                   (map (\j -> if ((wData w) U.! j == Burning) then 1 else 0) 
                        ns)
