{-# LANGUAGE ExistentialQuantification #-}

module Geometry (Ray(..)
                , eps
                , rayAt
                , rayEps
                , Hit(..)
                , Inter
                , intersection
                , Geometry(..)
                , geom
                , Shape(..)
                ) where

import Vec

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

