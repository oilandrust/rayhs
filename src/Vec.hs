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

data Vec = Vec { x :: !Double,
                 y :: !Double,
                 z :: !Double} deriving (Eq, Show)

data UV = UV { u :: !Double,
               v :: !Double } deriving (Eq, Show)

instance Num Vec where
  {-# INLINE (+) #-}
  (Vec x1 y1 z1) + (Vec x2 y2 z2) = Vec (x1+x2) (y1+y2) (z1+z2)
  {-# INLINE (*) #-}
  (Vec x1 y1 z1) * (Vec x2 y2 z2) = Vec (x1*x2) (y1*y2) (z1*z2)
  {-# INLINE (-) #-}
  (Vec x1 y1 z1) - (Vec x2 y2 z2) = Vec (x1-x2) (y1-y2) (z1-z2)
  {-# INLINE negate #-}
  negate (Vec x y z) = Vec (-x) (-y) (-z)

instance Num UV where
  {-# INLINE (+) #-}
  (UV x1 y1) + (UV x2 y2) = UV (x1+x2) (y1+y2)
  {-# INLINE (*) #-}
  (UV x1 y1) * (UV x2 y2) = UV (x1*x2) (y1*y2)
  {-# INLINE (-) #-}
  (UV x1 y1) - (UV x2 y2) = UV (x1-x2) (y1-y2)
  {-# INLINE negate #-}
  negate (UV x y) = UV (-x) (-y)

{- Exterior product -}
class Ext a where
  mul :: Double -> a -> a
  mult :: a -> Double -> a

instance Ext Vec where
  {-# INLINE mul #-}
  mul l (Vec x y z) = Vec (l*x) (l*y) (l*z)
  {-# INLINE mult #-}
  mult v l = mul l v

instance Ext UV where
  {-# INLINE mul #-}
  mul l (UV x y) = UV (l*x) (l*y)
  {-# INLINE mult #-}
  mult v l = mul l v

instance Ext Color where
  {-# INLINE mul #-}
  mul l (RGB x y z) = RGB (l*x) (l*y) (l*z)
  {-# INLINE mult #-}
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
{-# INLINE dot #-}
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec -> Vec -> Vec
{-# INLINE cross #-}
cross (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

sqrLen :: Vec -> Double
{-# INLINE sqrLen #-}
sqrLen v = dot v v

sqrDist :: Vec -> Vec -> Double
{-# INLINE sqrDist #-}
sqrDist v w = sqrLen (v - w)

dist :: Vec -> Vec -> Double
{-# INLINE dist #-}
dist v w = sqrt (sqrDist  v w)

normalize :: Vec -> Vec
{-# INLINE normalize #-}
normalize v = mul (1 / (sqrt . sqrLen $ v)) v

reflect :: Vec -> Vec -> Vec
{-# INLINE reflect #-}
reflect v n = v - mul (2 * dot v n) n

refract :: Vec -> Vec -> Double -> Double -> Maybe Vec
{-# INLINE refract #-}
refract i n n1 n2
  | sin20 > 1 = Nothing
  | otherwise = Just $ mul n1n2 i + mul coeff n
  where n1n2 = n1 / n2
        cos0 = -(dot i n)
        sin20 = n1n2 * n1n2 * (1 - cos0*cos0)
        coeff = n1n2 * cos0 - sqrt (1.0-sin20)

minV :: Vec -> Vec -> Vec
{-# INLINE minV #-}
minV (Vec a b c) (Vec x y z) = Vec (min a x) (min b y) (min c z)

maxV :: Vec -> Vec -> Vec
{-# INLINE maxV #-}
maxV (Vec a b c) (Vec x y z) = Vec (max a x) (max b y) (max c z)

{- Type conversions -}
fromList :: [Double] -> Vec
fromList d = Vec (head d) (d !! 1) (d !! 2)

toRGB :: Vec -> Color
toRGB (Vec x y z) = RGB x y z

fromRGB :: Color -> Vec
fromRGB (RGB x y z) = Vec x y z
