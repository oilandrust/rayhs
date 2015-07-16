{-# LANGUAGE ExistentialQuantification #-}

module Geometry (Ray(..)
                , rayAt
                , rayEps
                , Hit(..)
                , Inter
                , intersection
                , Geometry(..)
                , geom
                , Shape(..)
                , Mesh (..)
                )where

import Vec

import Data.List
import Data.Maybe
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

{- Ray -}
data Ray = Ray { origin :: Vec
               , direction :: Vec } deriving Show

rayAt :: Ray -> Double -> Vec
rayAt (Ray o d) t = o + Vec.mul t d

eps :: Double
eps = 0.0001

rayEps :: Vec -> Vec -> Ray
rayEps p n = Ray (p + (Vec.mul eps n)) n

{- Intersection hit -}
data Hit = Hit { p :: Vec
               , n :: Vec
               , t :: Double } deriving Show

{- Geometry Class -}
class Inter a  where
  intersection :: Ray -> a -> Maybe Hit

data Geometry = forall a. Inter a => Geometry a
geom :: Inter a => a -> Geometry
geom = Geometry

instance Inter Geometry where
  intersection ray (Geometry a) = intersection ray a

{- Ray/Primitives intersection -}

{- Basic shapes -}
data Shape = Plane Vec Vec
           | Sphere { center :: Vec, radius :: Double }

instance Inter Shape where
  intersection = rayShapeIntersection

rayShapeIntersection :: Ray -> Shape -> Maybe Hit
rayShapeIntersection ray@(Ray o d) (Plane p n)
  | (abs dDotn) > 0 && t > 0 = Just (Hit (rayAt ray t) n t)
  | otherwise = Nothing
    where dDotn = dot d n
          t = dot n (p - o) / dDotn
               
rayShapeIntersection ray@(Ray o d) (Sphere ct r)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (Hit p0 n0 t0)
  | t1 > 0 = Just (Hit p1 n1 t1)
  | otherwise = Nothing
  where delta = b ^ (2 :: Int) - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r ^ (2 :: Int)
        t0 = 0.5 * ((-b) - (sqrt delta)) / a
        p0 = rayAt ray t0
        n0 = normalize (p0 - ct)
        t1 = 0.5 * ((-b) + (sqrt delta)) / a
        p1 = rayAt ray t1
        n1 = normalize (p1 - ct)

{- Triangle Mesh -}
data Mesh = Mesh { vertices :: Vector Vec
                 , normals :: Vector Vec
                 , indices :: [Int] } deriving Show

instance Inter Mesh where
  intersection = rayInterMesh

rayInterMesh :: Ray -> Mesh -> Maybe Hit
rayInterMesh ray mesh@(Mesh pts _ ids) = do
  let hits = catMaybes $ mapTriangles (triangleIntersection ray) pts ids
    in case hits of
    [] -> Nothing
    xs -> Just $ minimumBy (compare `on` t) xs

triangleIntersection :: Ray -> (Vec, Vec, Vec) -> Maybe Hit
triangleIntersection ray@(Ray o d) (p0, p1, p2) = do
  case (abs(det) < eps ||
        u < 0 || u > 1 ||
        v < 0 || (u + v) > 1 ||
        t < eps) of
    True -> Nothing
    False -> Just $ Hit (rayAt ray t) n t 
  where e1 = p1 - p0
        e2 = p2 - p0
        p = cross d e2
        det = dot e1 p
        idet = 1 / det
        t0 = o - p0
        u = idet * (dot t0 p)
        q = cross t0 e1
        v = idet * (dot d q)
        t = idet * (dot e2 q)
        n = normalize (cross (p1 - p0) (p2 - p0))

mapTriangles :: ((Vec, Vec, Vec) -> b) -> Vector Vec -> [Int] -> [b]
mapTriangles f pts indices = map f
                             (map (\(i, j, k) -> (pts ! i, pts ! j, pts ! k))
                              (faces indices))
  where faces [] = []
        faces (i:j:k:is) = (i, j, k):(faces is)
