{-# LANGUAGE BangPatterns #-}
module DLA.Vec3 where

import DLA.Rmonad

data Vec3 = Vec3 {-# UNPACK #-}!Double {-# UNPACK #-}!Double {-# UNPACK #-}!Double
  deriving Show

--
-- for math, see : http://www.cs.cmu.edu/~mws/rpos.html
--
randUnitVec :: DLAMonad Vec3
{-# INLINE randUnitVec #-}
randUnitVec = do
  phi <- nextF (2.0 * pi)  -- phi ranges from 0.0 to 2.0*pi
  z <- nextF 2.0
  z <- return $ z - 1.0    -- z ranges from -r to r
  rct <- return $ sqrt( 1.0 - (z*z))
  return $ Vec3 (rct*(cos phi)) (rct*(sin phi)) z

randVec :: Double -> DLAMonad Vec3
{-# INLINE randVec #-}
randVec !r = do
  uv <- randUnitVec
  return $ vecScale uv r

vecAdd :: Vec3 -> Vec3 -> Vec3
vecAdd (Vec3 a b c) (Vec3 x y z) = Vec3 (a+x) (b+y) (c+z)

vecSub :: Vec3 -> Vec3 -> Vec3
vecSub (Vec3 a b c) (Vec3 x y z) = Vec3 (a-x) (b-y) (c-z)

vecScale :: Vec3 -> Double -> Vec3
vecScale (Vec3 a b c) !s = Vec3 (a*s) (b*s) (c*s)

vecDot :: Vec3 -> Vec3 -> Double
vecDot (Vec3 a b c) (Vec3 x y z) = (a*x)+(b*y)+(c*z)

vecNorm :: Vec3 -> Double
vecNorm v = sqrt (vecDot v v)

vecNormalize :: Vec3 -> Vec3
vecNormalize v = vecScale v (1.0 / (vecNorm v))

vecDimSelect :: Vec3 -> Int -> Double
vecDimSelect (Vec3 a b c) n =
    case (rem n 3) of
      0 -> a
      1 -> b
      2 -> c

vecLessThan :: Vec3 -> Vec3 -> Bool
vecLessThan (Vec3 a b c) (Vec3 x y z) =
  (a<x) && (b<y) && (c<z)

vecGreaterThan :: Vec3 -> Vec3 -> Bool
vecGreaterThan (Vec3 a b c) (Vec3 x y z) =
  (a>x) && (b>y) && (c>z)

