{-# LANGUAGE ExistentialQuantification #-}

module Geometry (Ray(..)
                , eps
                , rayAt
                , rayEps
                , Hit(..)
                , Inter
                , intersection
                , closestHit
                , Geometry(..)
                , geom
                , Shape(..)
                ) where

import Math
import Vec

import Data.List
import Data.Maybe
import Data.Function

{- Ray -}
data Ray = Ray { origin :: !Vec
               , direction :: !Vec } deriving Show

rayAt :: Ray -> Double -> Vec
{-# INLINE rayAt #-}
rayAt (Ray o d) t = o + Vec.mul t d

eps :: Double
eps = 0.0001

rayEps :: Vec -> Vec -> Ray
{-# INLINE rayEps #-}
rayEps p n = Ray (p + Vec.mul eps n) n

{- Intersection hit -}
data Hit = Hit { p :: !Position, n :: !Normal, uv :: !UV, t :: !Double }
         deriving Show

{- Geometry Class -}
class Inter a  where
  intersection :: Ray -> a -> Maybe Hit

data Geometry = forall a. Inter a => Geometry a
geom :: Inter a => a -> Geometry
geom = Geometry

instance Inter Geometry where
  intersection ray (Geometry a) = intersection ray a

closestHit :: [Maybe Hit] -> Maybe Hit
closestHit hits = case catMaybes hits of
  [] -> Nothing
  xs -> Just $ minimumBy (compare `on` t) xs

{- Ray/Primitives intersection -}

{- Basic shapes -}
data Shape = Plane { point :: !Vec, normal :: !Vec, tangent :: !Vec}
           | Sphere { center :: !Vec, radius :: !Double }

instance Inter Shape where
  intersection = rayShapeIntersection

rayShapeIntersection :: Ray -> Shape -> Maybe Hit
{-# INLINE rayShapeIntersection #-}
rayShapeIntersection ray@(Ray o d) (Plane p n t)
  | abs dDotn > 0 && time > 0 = Just (Hit pos n (UV u v) time)
  | otherwise = Nothing
    where dDotn = dot d n
          time = dot n (p - o) / dDotn
          pos = rayAt ray time --intersection
          b = cross t n
          rel = pos - p
          u = dot t rel
          v = dot b rel

rayShapeIntersection ray@(Ray o d) (Sphere ct r)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (Hit p0 n0 (polar n0) t0)
  | t1 > 0 = Just (Hit p1 n1 (polar n1) t1)
  | otherwise = Nothing
  where delta = b ^ (2 :: Int) - 4.0*a*c
        a = dot d d
        b = 2.0 * dot d (o - ct)
        c = sqrLen (o - ct) - r ^ (2 :: Int)
        t0 = 0.5 * ((-b) - sqrt delta) / a
        p0 = rayAt ray t0
        n0 = normalize (p0 - ct)
        t1 = 0.5 * ((-b) + sqrt delta) / a
        p1 = rayAt ray t1
        n1 = normalize (p1 - ct)
        polar p = UV (piInv * atan (z p / x p)) (piInv * acos (y p))
