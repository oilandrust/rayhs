module Material (ColorMap(..)
                , Material(..)
                , colorAt
                , r0
                , fresnel
                , diffuse) where

import Data.Fixed

import Color
import Math
import Vec

data ColorMap = Flat Color
              | CheckerBoard Color Color Double
              deriving Show

colorAt :: ColorMap -> UV -> Color
colorAt (Flat color) _ = color
colorAt (CheckerBoard c0 c1 scale) (UV u v) =
  if (mod' u scale - (0.5*scale)) * (mod' v scale - (0.5*scale)) < 0
  then c0
  else c1

data Material = Mirror { ior :: Double }
              | Diffuse { cd :: ColorMap }
              | Plastic { cd :: ColorMap, ior :: Double}
              | Emmit { ce :: Color }
              | Transparent { ior :: Double }
              | ShowNormal
              | ShowUV
              deriving Show

r0 :: Double -> Double -> Double
r0 n1 n2 = ((n1 - n2) / (n1 + n2)) ^ (2 :: Int)

fresnel :: Double -> Double -> Double
fresnel ior cos0 = r + (1 - r) * (1 - cos0) ^ (5 :: Int)
  where r = r0 1.0 ior

diffuse :: Color -> Color -> Vec -> Vec -> Color
diffuse cd lc l n = mul (max (dot l n) 0 * piInv) (cd * lc)