module Material (Material(Mirror, Diffuse, Plastic, Emmit, Transparent)
                , r0
                , fresnel
                , diffuse) where

import Color
import Math
import Vec

import qualified Vec (o, mul)
import qualified Color as C (mul)

data Material = Mirror { ior :: Float }
              | Diffuse { cd :: Color }
              | Plastic { cd :: Color, ior :: Float}
              | Emmit { ce :: Color }
              | Transparent { ior :: Float }
              deriving Show


r0 :: Float -> Float -> Float
r0 n1 n2 = ((n1 - n2) / (n1 + n2)) ^ (2 :: Int)

fresnel :: Float -> Float -> Float
fresnel ior cosθ = r + (1 - r) * (1 - cosθ) ^ (5 :: Int)
  where r = r0 1.0 ior
        
diffuse :: Color -> Color -> Vec -> Vec -> Color
diffuse cd lc l n = C.mul ((max (dot l n) 0) * piInv) (cd * lc)
