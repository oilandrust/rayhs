module Light (Light(..)
             , lightAt) where

import Color
import Vec

{- Light -}
data Light = Directional { dir :: !Vec, col :: !Color }
           | Point { pos :: !Vec, col :: !Color, r :: !Double } deriving Show

lightAt :: Light -> Vec -> (Vec, Color)
{-# INLINE lightAt #-}
lightAt (Directional d c) _ = (d, c)
lightAt (Point p0 c r) p = (Vec.mul (1/d) (p0 - p), mul falloff c)
  where falloff = 1.0 / (1.0 + d/r)^(2::Int)
        d = dist p0 p
