module Geometry (Ray(..)
                , rayAt
                , rayEps
                , Hit(..)
                , Geometry(..)
                , intersection
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

rayAt :: Ray -> Float -> Vec
rayAt (Ray o d) t = o + Vec.mul t d

eps :: Float
eps = 0.0001

rayEps :: Vec -> Vec -> Ray
rayEps p n = Ray (p + (Vec.mul eps n)) n

{- Intersection hit -}
data Hit = Hit { p :: Vec
               , n :: Vec
               , t :: Float } deriving Show

{- Geometry -}
data Geometry = Sphere { center :: Vec, radius :: Float }
              | Plane Vec Vec
              | Mesh { vertices :: Vector Vec
                     , normals :: Vector Vec
                     , indices :: [Int] }
              deriving Show

{- Ray/Primitives intersection -}
intersection :: Ray -> Geometry -> Maybe Hit
intersection ray@(Ray o d) (Plane p n)
  | (abs dDotn) > 0 && t > 0 = Just (Hit (rayAt ray t) n t)
  | otherwise = Nothing
    where dDotn = dot d n
          t = dot n (p - o) / dDotn

intersection ray@(Ray o d) (Sphere ct r)
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

intersection ray mesh@(Mesh pts _ ids) = do
  let hits = map fromJust $ filter isJust $
             (mapTriangles (triangleIntersection ray) pts ids)
    in case hits of
    [] -> Nothing
    xs ->  let hit = minimumBy (compare `on` t) xs
           in Just (Hit (p hit) (n hit) (t hit))

triangleIntersection :: Ray -> (Vec, Vec, Vec) -> Maybe Hit
triangleIntersection ray (p0, p1, p2) = do
  hit <- intersection ray (Plane p0 norm)
  let x = p hit
  case (inside x) of
    True -> Just hit
    False -> Nothing
  where norm = cross (p1 - p0) (p2 - p0)
        inside x = dot norm (cross (p1 - p0) (x - p0)) >= 0 &&
                   dot norm (cross (p2 - p1) (x - p1)) >= 0 &&
                   dot norm (cross (p0 - p2) (x - p2)) >= 0        

mapTriangles :: ((Vec, Vec, Vec) -> b) -> Vector Vec -> [Int] -> [b]
mapTriangles f pts indices = map f
                             (map (\(i, j, k) -> (pts ! i, pts ! j, pts ! k))
                              (faces indices))
  where faces [] = []
        faces (i:j:k:is) = (i, j, k):(faces is)
