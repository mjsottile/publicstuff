--
-- harmonic motion model
--
-- matthew sottile (matt@galois.com), 2011
--
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Simulate
import GHC.Float

-- window parameters
winWidth :: Int
winWidth  = 600

winHeight :: Int
winHeight = 600

cradius   = 3

-- physical parameters
eta = 0.0002
g = 9.80665
dt = 0.02

f :: Double -> Double -> Double -> Double -> Double
f theta omega t l = (-eta)*omega - (g/l)*sin(theta)

solve :: (Double, Double, Double, Double) -> IO (Double, Double, Double, Double)
solve (theta, omega, t, l) = do
  return (theta + (1/6)*(k1a + 2*k2a + 2*k3a + k4a),
          omega + (1/6)*(k1b + 2*k2b + 2*k3b + k4b),
          t',
          l)
  where
    t' = t + dt
    k1a = omega * dt
    k1b = (f theta omega t' l) * dt
    k2a = (omega + k1b/2) * dt
    k2b = (f (theta + k1a/2) (omega + k1b/2) (t' + dt/2) l) * dt
    k3a = (omega + k2b/2) * dt
    k3b = (f (theta + k2a/2) (omega + k2b/2) (t' + dt/2) l) * dt
    k4a = (omega + k3b) * dt
    k4b = (f (theta + k3a) (omega + k3b) (t' + dt) l) * dt

clr = makeColor 1.0 1.0 0.0 1.0
lineclr = makeColor 0.3 0.3 0.3 0.5

renderPendulum :: (Double, Double, Double, Double) -> Picture
renderPendulum (theta, omega, t, l) =
  let x = double2Float $ l*sin theta
      y = double2Float $ -(l*cos theta)
      twidth = ((fromIntegral winWidth) / 2)-15
      theight = ((fromIntegral winHeight) / 2)-15
  in
    Pictures $ [
      Color lineclr $
        Line [(0,0), (x*twidth*3,y*theight*3)],
      Color clr $
        Translate (x*twidth*3) (y*theight*3) $
        Circle cradius ]

renderPendulums ps = do return (Pictures $ map renderPendulum ps)

lengths :: Int -> Double -> Double -> [Double]
lengths n t l =
  let thelen curn = (t/((l+((fromIntegral n)-curn)) * 2.0 * pi))**2 * g
  in
      [thelen (fromIntegral i) | i <- [1..n]]

main :: IO ()
main = do
  let niter = 1000
      theta0 = pi / 8
      omega0 = 0
      t = 0
      npendu = 15
      starts = map (\i -> (theta0, omega0, t, i)) (lengths npendu 54.0 60.0)
  simulateIO
    (InWindow "Pendulums" (winWidth, winHeight) (1, 1))
    (greyN 0.1)
    45
    starts
    renderPendulums
    (\vp f m -> mapM solve m)
