module Vec (Vec(Vec)
           , o
           , xAxis
           , yAxis
           , zAxis
           , mul
           , dot
           , sqrLen
           , sqrDist
           , dist
           , normalize
           , reflect
           , toRGB
           , fromRGB) where
import Color hiding (mul)

data Vec = Vec Float Float Float deriving (Eq, Show)
instance Num Vec where
  (Vec x1 y1 z1) + (Vec x2 y2 z2) = (Vec (x1+x2) (y1+y2) (z1+z2))
  (Vec x1 y1 z1) * (Vec x2 y2 z2) = (Vec (x1*x2) (y1*y2) (z1*z2))
  (Vec x1 y1 z1) - (Vec x2 y2 z2) = (Vec (x1-x2) (y1-y2) (z1-z2))
  negate (Vec x y z) = (Vec (-x) (-y) (-z))  

o :: Vec
o = Vec 0 0 0

xAxis :: Vec
xAxis = Vec 1 0 0

yAxis :: Vec
yAxis = Vec 0 1 0

zAxis :: Vec
zAxis = Vec 0 0 1

mul :: Float -> Vec -> Vec
mul l (Vec x y z) = Vec (l*x) (l*y) (l*z)

dot :: Vec -> Vec -> Float
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

sqrLen :: Vec -> Float
sqrLen v = dot v v

sqrDist :: Vec -> Vec -> Float
sqrDist v w = sqrLen (v - w)

dist :: Vec -> Vec -> Float
dist v w = sqrt (sqrDist  v w)

normalize :: Vec -> Vec
normalize v = mul (1 / (sqrt . sqrLen $ v)) v

toRGB :: Vec -> Color
toRGB (Vec x y z) = (RGB x y z)

fromRGB :: Color -> Vec
fromRGB (RGB x y z) = (Vec x y z)

reflect :: Vec -> Vec -> Vec
reflect v n = v - (Vec.mul (2 * (dot v n)) n)
