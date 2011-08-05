{-# LANGUAGE BangPatterns #-}
--
-- KDTree code
--
-- mjsottile@computer.org
--
module KDTree2d (
  KDTreeNode(..),
  newKDTree,
  kdtAddPoints,
  kdtAddPoint,
  kdtRangeSearch,
  kdtCollisionDetect,
  kdtInBounds,
  dumpKDTree,
  mapKDTree,
  kdtreeToList
) where

import Vec2
import Data.Maybe
import System.IO

data KDTreeNode a =
    Empty
  | Node !(KDTreeNode a) !Vec2 !a !(KDTreeNode a)
  deriving Show

kdtreeToListOld :: (KDTreeNode a) -> [a]
kdtreeToListOld Empty = []
kdtreeToListOld (Node l _ x r) = [x]++(kdtreeToList l)++(kdtreeToList r)

kdtreeToList :: (KDTreeNode a) -> [a]
kdtreeToList k = mapKDTree k id

mapKDTreeOld :: KDTreeNode a -> (a -> b) -> [b]
mapKDTreeOld Empty _ = []
mapKDTreeOld (Node l p n r) f = (f n):((mapKDTreeOld l f)++(mapKDTreeOld r f))

mapKDTree :: KDTreeNode a -> (a -> b) -> [b]
mapKDTree Empty _ = []
mapKDTree (Node Empty p n r) f = (f n) : (mapKDTree r f)
mapKDTree (Node (Node l1 p1 n1 r1) p n r) f = mapKDTree (Node l1 p1 n1 (Node r1 p n r)) f

printVec :: Vec2 -> Handle -> Int -> IO ()
printVec (Vec2 x y) h i = do 
  hPutStrLn h $ (show i)++" "++(show x)++" "++(show y)

dumpKDTree :: KDTreeNode Int -> String -> IO ()
dumpKDTree kdt name = do
  h <- openFile name WriteMode
  hPutStrLn h "n x y z"
  dumpKDTreeInner kdt h
  hClose h

dumpKDTreeInner :: KDTreeNode Int -> Handle -> IO ()
dumpKDTreeInner kdt h = do
  case kdt of
    Empty -> return ()
    Node l v d r -> do printVec v h d
                       dumpKDTreeInner l h
                       dumpKDTreeInner r h

newKDTree :: KDTreeNode a
newKDTree = Empty

kdtAddWithDepth :: (KDTreeNode a) -> Vec2 -> a -> Int -> (KDTreeNode a)
kdtAddWithDepth Empty pos dat _ = Node Empty pos dat Empty
kdtAddWithDepth (Node left npos ndata right) pos dat d =
  if (vecDimSelect pos d) < (vecDimSelect npos d) then
    Node (kdtAddWithDepth left pos dat d') npos ndata right
  else
    Node left npos ndata (kdtAddWithDepth right pos dat d')
  where d' = if (d == 1) then 0 else 1

kdtAddPoint :: (KDTreeNode a) -> Vec2 -> a -> (KDTreeNode a)
kdtAddPoint t p d = kdtAddWithDepth t p d 0

kdtInBounds p bMin bMax =
  (vecLessThan p bMax) && (vecGreaterThan p bMin)

-- X dimension
kdtRangeSearchRecX :: (KDTreeNode a) -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearchRecX Empty _ _ = []
kdtRangeSearchRecX (Node left npos ndata right) bMin bMax =
  if nc < mnc then
    nextfun right bMin bMax
  else
    if nc > mxc then
      nextfun left bMin bMax
    else
      if (kdtInBounds npos bMin bMax) then
        (npos,ndata) : ((nextfun right bMin bMax)++
                        (nextfun left bMin bMax))
      else
        (nextfun right bMin bMax)++
        (nextfun left bMin bMax)
  where Vec2 nc _  = npos
        Vec2 mnc _ = bMin
        Vec2 mxc _ = bMax
        nextfun = kdtRangeSearchRecY

-- Y dimension
kdtRangeSearchRecY :: (KDTreeNode a) -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearchRecY Empty _ _ = []
kdtRangeSearchRecY (Node left npos ndata right) bMin bMax =
  if nc < mnc then
    nextfun right bMin bMax
  else
    if nc > mxc then
      nextfun left bMin bMax
    else
      if (kdtInBounds npos bMin bMax) then
        (npos,ndata) : ((nextfun right bMin bMax)++
                        (nextfun left bMin bMax))
      else
        (nextfun right bMin bMax)++
        (nextfun left bMin bMax)
  where Vec2 _ nc  = npos
        Vec2 _ mnc = bMin
        Vec2 _ mxc = bMax
        nextfun = kdtRangeSearchRecX

kdtRangeSearch :: (KDTreeNode a) -> Vec2 -> Vec2 -> [(Vec2,a)]
kdtRangeSearch t bMin bMax =
  kdtRangeSearchRecX t bMin bMax

kdtAddPoints :: [(Vec2,a)] -> (KDTreeNode a) -> (KDTreeNode a)
kdtAddPoints [] t = t
kdtAddPoints ((pt,dat):ps) t =
  kdtAddPoints ps tt
  where tt = kdtAddPoint t pt dat

singleCollision :: Vec2 -> Vec2 -> Vec2 -> Double -> a -> Maybe (Vec2,a)
singleCollision pt start a eps dat =
  if (sqrd_dist < eps*eps) then Just (vecAdd start p, dat)
                           else Nothing
  where
    b = vecSub pt start
    xhat = (vecDot a b) / (vecDot a a)
    p = vecScale a xhat
    e = vecSub p b
    sqrd_dist = vecDot e e

kdtCollisionDetect :: (KDTreeNode a) -> Vec2 -> Vec2 -> Double -> 
                      [(Vec2,a)]
kdtCollisionDetect root !start !end !eps =
    map fromJust $ filter (\i -> isJust i) colls
    where
      Vec2 sx sy = start
      Vec2 ex ey = end
      rmin = Vec2 ((min sx ex)-eps) ((min sy ey)-eps)
      rmax = Vec2 ((max sx ex)+eps) ((max sy ey)+eps)
      pts = kdtRangeSearch root rmin rmax
      a = vecSub end start
      colls = map (\(pt,dat) -> (singleCollision pt start a eps dat)) pts
      
