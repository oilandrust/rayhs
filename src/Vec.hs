module Vec (Vec (..)
           , Ext (..)
           , Position
           , Normal
           , Direction
           , UV (..)
           , o
           , xAxis
           , yAxis
           , zAxis
           , dot
           , cross
           , sqrLen
           , sqrDist
           , dist
           , normalize
           , reflect
           , refract
           , minV
           , maxV
           , toRGB
           , fromRGB
           , fromList) where

import Color

data Vec = Vec { x :: Double,
                 y :: Double,
                 z :: Double} deriving (Eq, Show)

data UV = UV { u :: Double,
               v :: Double } deriving (Eq, Show)

instance Num Vec where
  (Vec x1 y1 z1) + (Vec x2 y2 z2) = (Vec (x1+x2) (y1+y2) (z1+z2))
  (Vec x1 y1 z1) * (Vec x2 y2 z2) = (Vec (x1*x2) (y1*y2) (z1*z2))
  (Vec x1 y1 z1) - (Vec x2 y2 z2) = (Vec (x1-x2) (y1-y2) (z1-z2))
  negate (Vec x y z) = (Vec (-x) (-y) (-z))  

instance Num UV where
  (UV x1 y1) + (UV x2 y2) = (UV (x1+x2) (y1+y2))
  (UV x1 y1) * (UV x2 y2) = (UV (x1*x2) (y1*y2))
  (UV x1 y1) - (UV x2 y2) = (UV (x1-x2) (y1-y2))
  negate (UV x y) = (UV (-x) (-y))  

{- Exterior product -}
class Ext a where
  mul :: Double -> a -> a
  mult :: a -> Double -> a

instance Ext Vec where
  mul l (Vec x y z) = Vec (l*x) (l*y) (l*z)
  mult v l = mul l v

instance Ext UV where
  mul l (UV x y) = UV (l*x) (l*y)
  mult v l = mul l v

instance Ext Color where
  mul l (RGB x y z) = RGB (l*x) (l*y) (l*z)
  mult v l = mul l v

type Position = Vec
type Normal = Vec
type Direction = Vec

o :: Vec
o = Vec 0 0 0

xAxis :: Vec
xAxis = Vec 1 0 0

yAxis :: Vec
yAxis = Vec 0 1 0

zAxis :: Vec
zAxis = Vec 0 0 1

{- Functions -}
dot :: Vec -> Vec -> Double
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec -> Vec -> Vec
cross (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

sqrLen :: Vec -> Double
sqrLen v = dot v v

sqrDist :: Vec -> Vec -> Double
sqrDist v w = sqrLen (v - w)

dist :: Vec -> Vec -> Double
dist v w = sqrt (sqrDist  v w)

normalize :: Vec -> Vec
normalize v = mul (1 / (sqrt . sqrLen $ v)) v

reflect :: Vec -> Vec -> Vec
reflect v n = v - (Vec.mul (2 * (dot v n)) n)

refract :: Vec -> Vec -> Double -> Double -> Maybe Vec
refract i n n1 n2
  | sin2θ > 1 = Nothing
  | otherwise = Just $ (Vec.mul n1n2 i)
                + (Vec.mul coeff n)
  where n1n2 = n1 / n2
        cosθ = (-(dot i n))
        sin2θ = n1n2 * n1n2 * (1 - cosθ*cosθ)
        coeff = n1n2 * cosθ - (sqrt (1.0-sin2θ))

minV :: Vec -> Vec -> Vec
minV (Vec a b c) (Vec x y z) = Vec (min a x) (min b y) (min c z)

maxV :: Vec -> Vec -> Vec
maxV (Vec a b c) (Vec x y z) = Vec (max a x) (max b y) (max c z)

{- Type conversions -}
fromList :: [Double] -> Vec
fromList d = Vec (d !! 0) (d !! 1) (d !! 2)

toRGB :: Vec -> Color
toRGB (Vec x y z) = (RGB x y z)

fromRGB :: Color -> Vec
fromRGB (RGB x y z) = (Vec x y z)
