module Material (Material(..)
                , r0
                , fresnel
                , diffuse) where

import Data.Fixed

import Color
import ColorMap
import Math
import Vec

data Material = Mirror { ior :: !Double }
              | Diffuse { cd :: !ColorMap }
              | Plastic { cd :: !ColorMap, ior :: !Double}
              | Emmit { ce :: !Color }
              | Transparent { ior :: !Double }
              | ShowNormal
              | ShowUV
              deriving Show

r0 :: Double -> Double -> Double
{-# INLINE r0 #-}
r0 n1 n2 = ((n1 - n2) / (n1 + n2)) ^ (2 :: Int)

fresnel :: Double -> Double -> Double
{-# INLINE fresnel #-}
fresnel ior cos0 = r + (1 - r) * (1 - cos0) ^ (5 :: Int)
  where r = r0 1.0 ior

diffuse :: Color -> Color -> Vec -> Vec -> Color
{-# INLINE diffuse #-}
diffuse cd lc l n = mul (max (dot l n) 0 * piInv) (cd * lc)
