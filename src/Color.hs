{-# LANGUAGE DeriveGeneric #-}

module Color (Color(RGB)
             , rgbi
             , black
             , white
             , red
             , green
             , blue
             , gray
             , pink
             , yellow) where

import Math

import GHC.Generics

{- Color -}
data Color = RGB !Double !Double !Double deriving (Show, Generic)

instance Num Color where
  {-# INLINE (+) #-}
  (RGB x1 y1 z1) + (RGB x2 y2 z2) = RGB (x1+x2) (y1+y2) (z1+z2)
  {-# INLINE (*) #-}
  (RGB x1 y1 z1) * (RGB x2 y2 z2) = RGB (x1*x2) (y1*y2) (z1*z2)
  {-# INLINE (-) #-}
  (RGB x1 y1 z1) - (RGB x2 y2 z2) = RGB (x1-x2) (y1-y2) (z1-z2)
  {-# INLINE negate #-}
  negate (RGB x y z) = RGB (-x) (-y) (-z)

rgbi :: Int -> Int -> Int -> Color
{-# INLINE rgbi #-}
rgbi r g b = RGB (fdiv r 255) (fdiv r 255) (fdiv r 255)

black :: Color
black = RGB 0 0 0

white :: Color
white = RGB 1 1 1

red :: Color
red = RGB 1 0 0

green :: Color
green = RGB 0 1 0

blue :: Color
blue = RGB 0 0 1

gray :: Double -> Color
gray g = RGB g g g

pink :: Color
pink = RGB 1 0.8  1

yellow :: Color
yellow = RGB 1 1 0.6

mul :: Double -> Color -> Color
{-# INLINE mul #-}
mul v (RGB r g b) = RGB (v*r) (v*g) (v*b)
