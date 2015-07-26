module KDTree (KDTree(..)
              , buildKDTree) where

import Math
import Vec
import Geometry
import Mesh

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

{- Bounding Box -}
data Box = Box { lower :: !Vec
               , upper :: !Vec } deriving Show

empty :: Box
empty = Box (Vec inf inf inf) (Vec (-inf) (-inf) (-inf))

include :: Box -> Vec -> Box
include (Box min max) p = Box (minV min p) (maxV  max p)

includeTriangle :: Box -> Triangle -> Box
includeTriangle box (Triangle
                     (Vertex a _ _)
                     (Vertex b _ _)
                     (Vertex c _ _)) = include (include (include box a) b) c

buildBoundingBox :: [Triangle] -> Box
buildBoundingBox triangles = foldl includeTriangle empty triangles

max3 :: Double -> Double -> Double -> Double
{-# INLINE max3 #-}
max3 a b = max (max a b)

min3 :: Double -> Double -> Double -> Double
{-# INLINE min3 #-}
min3 a b = min (min a b)

rayInterBox :: Ray -> Box -> Maybe Double
{-# INLINE rayInterBox #-}
rayInterBox (Ray (Vec ox oy oz) (Vec dx dy dz))
  (Box (Vec mx my mz) (Vec mX mY mZ)) =
  if tmax < 0 || tmin > tmax
  then Nothing
  else Just tmin
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
            | Empty

instance Show KDTree where
  show (Leaf b tris) = "Leaf [" ++ (show $ length tris) ++ "]"
  show (Node b l r) = "Node (" ++ show l ++ ", " ++ show r ++ ")"
  show Empty = "Empty"

buildKDTree :: Mesh -> KDTree
buildKDTree mesh = buildNode (triangles mesh) 0 0

baryCenter :: Triangle -> Vec
baryCenter (Triangle (Vertex a _ _)
                     (Vertex b _ _)
                     (Vertex c _ _)) = mul (1 / 3) (a + b + c)

maxDepth :: Int
maxDepth = 10

buildNode :: [Triangle] -> Int -> Int -> KDTree
buildNode tris depth axis
  | null tris = Empty
  | length tris < 5 || depth >= maxDepth = Leaf box tris
  | otherwise = Node box left right
  where box = buildBoundingBox tris
        left = buildNode [t | t <- tris, (baryCenter t) @@ axis < split]
               (depth + 1) nextAxis
        right = buildNode [t | t <- tris, split <= (baryCenter t) @@ axis]
                (depth + 1) nextAxis
        split = 0.5 * ((upper box) @@ axis + (lower box) @@ axis)
        nextAxis = (axis + 1) `mod` 3

{- Intersection -}
instance Inter KDTree where
  intersection = rayInter

rayInter :: Ray -> KDTree -> Maybe Hit
{-# INLINE rayInter #-}
rayInter ray (Leaf bbox triangles) = case rayInterBox ray bbox of
  Nothing -> Nothing
  Just _ -> closestHit $ map (triangleIntersection ray) triangles

rayInter ray (Node bbox left right) = case rayInterBox ray bbox of
  Nothing -> Nothing
  Just _ -> let lHit = rayInter ray left
                rHit = rayInter ray right
            in minMaybeHit lHit rHit
rayInter _ Empty = Nothing

minMaybeHit :: Maybe Hit -> Maybe Hit -> Maybe Hit
minMaybeHit Nothing Nothing = Nothing
minMaybeHit (Just hit) Nothing = Just hit
minMaybeHit Nothing (Just hit) = Just hit
minMaybeHit (Just h1) (Just h2) = if t h1 < t h2
                                  then Just h1
                                  else Just h2
