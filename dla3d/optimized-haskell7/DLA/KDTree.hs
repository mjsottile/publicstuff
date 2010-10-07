{-# LANGUAGE BangPatterns #-}
--
-- KDTree code
--
-- mjsottile@computer.org
--
module DLA.KDTree (
  KDTreeNode(..),
  newKDTree,
  kdtAddPoints,
  kdtRangeSearch,
  kdtCollisionDetect,
  kdtInBounds,
  dumpKDTree
) where

import DLA.Vec3
import Data.Maybe
import System.IO

data KDTreeNode a =
    Empty
  | Node !(KDTreeNode a) !Vec3 !a !(KDTreeNode a)
  deriving Show

printVec :: Vec3 -> Handle -> Int -> IO ()
printVec (Vec3 x y z) h i = do 
  hPutStrLn h $ (show i)++" "++(show x)++" "++(show y)++" "++(show z)

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

kdtAddWithDepth :: (KDTreeNode a) -> Vec3 -> a -> Int -> (KDTreeNode a)
kdtAddWithDepth Empty pos dat _ = Node Empty pos dat Empty
kdtAddWithDepth (Node left npos ndata right) pos dat d =
  if (vecDimSelect pos d) < (vecDimSelect npos d) then
    Node (kdtAddWithDepth left pos dat d') npos ndata right
  else
    Node left npos ndata (kdtAddWithDepth right pos dat d')
  where d' = if (d == 2) then 0 else d+1

kdtAddPoint :: (KDTreeNode a) -> Vec3 -> a -> (KDTreeNode a)
kdtAddPoint t p d = kdtAddWithDepth t p d 0

kdtInBounds p bMin bMax =
  (vecLessThan p bMax) && (vecGreaterThan p bMin)

-- X dimension
kdtRangeSearchRecX :: (KDTreeNode a) -> Vec3 -> Vec3 -> [(Vec3,a)]
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
  where Vec3 nc _ _  = npos
        Vec3 mnc _ _ = bMin
        Vec3 mxc _ _ = bMax
        nextfun = kdtRangeSearchRecY

-- Y dimension
kdtRangeSearchRecY :: (KDTreeNode a) -> Vec3 -> Vec3 -> [(Vec3,a)]
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
  where Vec3 _ nc _  = npos
        Vec3 _ mnc _ = bMin
        Vec3 _ mxc _ = bMax
        nextfun = kdtRangeSearchRecZ

-- Z dimension
kdtRangeSearchRecZ :: (KDTreeNode a) -> Vec3 -> Vec3 -> [(Vec3,a)]
kdtRangeSearchRecZ Empty _ _ = []
kdtRangeSearchRecZ (Node left npos ndata right) bMin bMax =
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
  where Vec3 _ _ nc  = npos
        Vec3 _ _ mnc = bMin
        Vec3 _ _ mxc = bMax
        nextfun = kdtRangeSearchRecX


kdtRangeSearch :: (KDTreeNode a) -> Vec3 -> Vec3 -> [(Vec3,a)]
kdtRangeSearch t bMin bMax =
  kdtRangeSearchRecX t bMin bMax

kdtAddPoints :: [(Vec3,a)] -> (KDTreeNode a) -> (KDTreeNode a)
kdtAddPoints [] t = t
kdtAddPoints ((pt,dat):ps) t =
  kdtAddPoints ps tt
  where tt = kdtAddPoint t pt dat

singleCollision :: Vec3 -> Vec3 -> Vec3 -> Double -> a -> Maybe (Vec3,a)
singleCollision pt start a eps dat =
  if (sqrd_dist < eps*eps) then Just (vecAdd start p, dat)
                           else Nothing
  where
    b = vecSub pt start
    xhat = (vecDot a b) / (vecDot a a)
    p = vecScale a xhat
    e = vecSub p b
    sqrd_dist = vecDot e e

kdtCollisionDetect :: (KDTreeNode a) -> Vec3 -> Vec3 -> Double -> 
                      [(Vec3,a)]
kdtCollisionDetect root !start !end !eps =
    map fromJust $ filter (\i -> isJust i) colls
    where
      Vec3 sx sy sz = start
      Vec3 ex ey ez = end
      rmin = Vec3 ((min sx ex)-eps) ((min sy ey)-eps) ((min sz ez)-eps)
      rmax = Vec3 ((max sx ex)+eps) ((max sy ey)+eps) ((max sz ez)+eps)
      pts = kdtRangeSearch root rmin rmax
      a = vecSub end start
      colls = map (\(pt,dat) -> (singleCollision pt start a eps dat)) pts
      