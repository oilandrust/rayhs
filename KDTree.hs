module KDTree (KDTree(..)
              , buildKDTree) where

import Math
import Vec
import Geometry
import Mesh

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

{- Bounding Box -}
data Box = Box Vec Vec deriving Show

empty :: Box
empty = Box (Vec inf inf inf) (Vec (-inf) (-inf) (-inf))

include :: Box -> Vec -> Box
include (Box min max) p = Box (minV min p) (maxV  max p) 

buildBoundingBox :: Mesh -> Box
buildBoundingBox mesh = V.foldl (\box (p, _, _) -> include box p)
                        empty (vertices mesh) 

max3 :: Double -> Double -> Double -> Double
max3 a b c = (max (max a b) c)

min3 :: Double -> Double -> Double -> Double
min3 a b c = (min (min a b) c)

rayInterBox :: Ray -> Box -> Maybe Double
rayInterBox (Ray (Vec ox oy oz) (Vec dx dy dz))
  (Box (Vec mx my mz) (Vec mX mY mZ)) =
  case (tmax < 0 || tmin > tmax) of
    True -> Nothing
    False -> Just tmin
  where idx = 1 / dx
        idy = 1 / dy
        idz = 1 / dz
        t1 = idx * (mx - ox)
        t2 = idx * (mX - ox)
        t3 = idy * (my - oy)
        t4 = idy * (mY - oy)
        t5 = idz * (mz - oz)
        t6 = idz * (mZ - oz)        
        tmin = max3 (min t1 t2) (min t3 t4) (min t5 t6)
        tmax = min3 (max t1 t2) (max t3 t4) (max t5 t6)        

{- KDTree -}
data KDTree = Leaf Box [Triangle]
            | Node Box KDTree KDTree

buildKDTree :: Mesh -> KDTree
buildKDTree mesh = Leaf (buildBoundingBox mesh) (triangles mesh)

{- Intersection -}
instance Inter KDTree where
  intersection = rayInter

rayInter :: Ray -> KDTree -> Maybe Hit
rayInter ray (Leaf bbox triangles) = case (rayInterBox ray bbox) of
  Nothing -> Nothing
  Just _ -> closestHit $ map (triangleIntersection ray) triangles
  


