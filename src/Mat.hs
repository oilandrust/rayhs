module Mat (Mat3(..)
           , identity
           , scale
           , rotateX
           , rotateY
           , rotateZ
           , rotate
           , fromColumns
           , transpose
           , lookAt
           , apply) where

import Vec

data Mat3 = Mat3
            !Double !Double !Double
            !Double !Double !Double
            !Double !Double !Double

instance Num Mat3 where
  {-# INLINE (+) #-}
  (Mat3 a1 b1 c1 d1 e1 f1 g1 h1 i1)
    + (Mat3 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    Mat3 (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2) (f1+f2) (g1+g2) (h1+h2) (i1+i2)
  {-# INLINE (*) #-}
  (Mat3 a1 b1 c1 d1 e1 f1 g1 h1 i1)
    * (Mat3 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    Mat3
    (a1*a2 + b1*d2 + c1*g2) (a1*b2 + b1*e2 + c1*h2) (a1*c2 + b1*f2 + c1*i2)
    (d1*a2 + e1*d2 + f1*g2) (d1*b2 + e1*e2 + f1*h2) (d1*c2 + e1*f2 + f1*i2)
    (g1*a2 + h1*d2 + i1*g2) (g1*b2 + h1*e2 + i1*h2) (g1*c2 + h1*f2 + i1*i2)
  {-# INLINE (-) #-}
  (Mat3 a1 b1 c1 d1 e1 f1 g1 h1 i1)
    - (Mat3 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
    Mat3 (a1-a2) (b1-b2) (c1-c2) (d1-d2) (e1-e2) (f1-f2) (g1-g2) (h1-h2) (i1-i2)
  {-# INLINE negate #-}
  negate (Mat3 a1 b1 c1 d1 e1 f1 g1 h1 i1) =
    Mat3 (-a1) (-b1) (-c1) (-d1) (-e1) (-f1) (-g1) (-h1) (-i1)

apply :: Mat3 -> Vec -> Vec
apply (Mat3 a b c d e f g h i) (Vec x y z) = Vec v1 v2 v3
  where v1 = a*x + b*y + c*z
        v2 = d*x + e*y + f*z
        v3 = g*x + h*y + i*z

{- transpose -}
transpose :: Mat3 -> Mat3
transpose (Mat3 a b c d r f g h i) =
  Mat3 a d g b r h c f i

{- constructors -}
identity :: Mat3
identity = Mat3 1 0 0 0 1 0 0 0 1

scale :: Vec -> Mat3
scale (Vec sx sy sz) = Mat3 sx 0 0 0 sy 0 0 0 sz

rotateZ :: Double -> Mat3
rotateZ angle = Mat3
                (cos angle) (-(sin angle)) 0
                (sin angle) (cos angle) 0
                0 0 1

rotateY :: Double -> Mat3
rotateY angle = Mat3
                (cos angle) 0 (sin angle)
                0 1 0
                (-(sin angle)) 0 (cos angle)

rotateX :: Double -> Mat3
rotateX angle = Mat3
                1 0 0
                0 (cos angle) (-(sin angle))
                0 (sin angle) (cos angle)

rotate :: Vec -> Double -> Mat3
rotate axis angle = mt * rot * m
  where (r, s, t) = orthonormal axis
        m = fromColumns r s t
        mt = transpose m
        rot = rotateX angle

fromColumns :: Vec -> Vec -> Vec -> Mat3
fromColumns v1 v2 v3 = Mat3
                       (x v1) (x v2) (x v3)
                       (y v1) (y v2) (y v3)
                       (z v1) (z v2) (z v3)

lookAt :: Vec -> Vec -> Vec -> Mat3
lookAt pos target tup = fromColumns right up forward
  where forward = normalize $ target - pos
        right = normalize $ cross tup forward
        up = cross forward right
