--
-- 2d opengl program, forest fire
--
-- based on various cobbled together pieces of found code and experiments.
-- 
-- matt@galois.com
--
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Debug.Trace
import System.Random
import System.Posix.Unistd
import Control.Concurrent
import System.Random.Mersenne.Pure64
import Data.IORef
import qualified ForestFire as FF
import qualified Data.Vector as U

-- window dimensions
initialWindowSizeX :: Int
initialWindowSizeX = 300

initialWindowSizeY :: Int
initialWindowSizeY = 300

-- we want to split our window into initialWindowSizeX/pixelsPerUnit grid
-- squares wide, and initialWindowSizeY/pixelsPerUnit grid squares high
pixelsPerUnit = 4

-- PRNG seed
seed = 2

main = do
  getArgsAndInitialize
  let w = (fromIntegral initialWindowSizeX :: GLsizei)
  let h = (fromIntegral initialWindowSizeY :: GLsizei)
  initialDisplayMode $= [RGBMode, DoubleBuffered]
  initialWindowSize $= Size w h
  (Size screenSizeX screenSizeY) <- get screenSize
  let initialPos = Position x y where
      x = (screenSizeX - w) `div` 2
      y = (screenSizeY - h) `div` 2
  initialWindowPosition $= initialPos
  createWindow "Forest Fire!"
  let worldSize = FF.Size (initialWindowSizeX `div` pixelsPerUnit) 
                          (initialWindowSizeY `div` pixelsPerUnit)
  world <- newIORef (FF.initialize worldSize)
  rngstate <- newIORef (pureMT seed)
  displayCallback $= display world
  reshapeCallback $= Nothing
  idleCallback $= Just (threadBody world rngstate)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 ((fromIntegral w) / (fromIntegral pixelsPerUnit)) 0 ((fromIntegral h) / (fromIntegral pixelsPerUnit))
  mainLoop

threadBody world rngstate = do
  wval <- readIORef world
  wval' <- FF.oneStep wval rngstate 0.96 0.999
  writeIORef world wval'
  postRedisplay Nothing

display world = do
  clear [ColorBuffer]
  wval <- readIORef world
  let (FF.Size w h) = FF.wSize wval
  let points = foldl (++) [] $ map (\i -> map (\j -> (i,j)) [0..(h-1)]) [0..(w-1)]
  drawSquares points wval
  swapBuffers

tree  = Color4 0.7 0.7 0.7  1.0
empty = Color4 0.2 0.3 0.6  1.0
burn  = Color4 0.9 0.2 0.2  1.0

drawSquares :: [(Int,Int)] -> FF.World -> IO ()
drawSquares pts world = do
  renderPrimitive Quads $ mapM_ drawQuad pts
  where
      wdat = FF.wData world
      wsiz = FF.wSize world
      drawQuad (x, y) = do
          currentColor $= c
          vertex $ Vertex2 x0 y0
          vertex $ Vertex2 x1 y0
          vertex $ Vertex2 x1 y1
          vertex $ Vertex2 x0 y1
          where
              x0 :: GLint
              x0 = fromIntegral x
              x1 = fromIntegral x + 1
              y0 = fromIntegral y
              y1 = fromIntegral y + 1
              c = case (wdat U.! (FF.idx x y wsiz)) of
                    FF.Burning -> burn
                    FF.Empty   -> empty
                    FF.Tree    -> tree

