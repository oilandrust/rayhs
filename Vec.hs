module Vec (Vec(Vec)
           , o
           , xAxis
           , yAxis
           , zAxis
           , mul
           , dot
           , cross
           , sqrLen
           , sqrDist
           , dist
           , normalize
           , reflect
           , refract
           , toRGB
           , fromRGB
           , fromList) where
import Color hiding (mul)

data Vec = Vec Double Double Double deriving (Eq, Show)
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

mul :: Double -> Vec -> Vec
mul l (Vec x y z) = Vec (l*x) (l*y) (l*z)

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

{- Type conversions -}
fromList :: [Double] -> Vec
fromList d = Vec (d !! 0) (d !! 1) (d !! 2)

toRGB :: Vec -> Color
toRGB (Vec x y z) = (RGB x y z)

fromRGB :: Color -> Vec
fromRGB (RGB x y z) = (Vec x y z)
