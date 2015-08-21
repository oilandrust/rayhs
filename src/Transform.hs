module Transform (Mat3(..)
                 , mat3
                 , lookAt
                 , transform) where

import Vec

data Mat3 = Mat3
            !Double !Double !Double
            !Double !Double !Double
            !Double !Double !Double

mat3 :: Vec -> Vec -> Vec -> Mat3
mat3 v1 v2 v3 = Mat3
                (x v1) (x v2) (x v3)
                (y v1) (y v2) (y v3)
                (z v1) (z v2) (z v3)

lookAt :: Vec -> Vec -> Vec -> Mat3
lookAt pos target tup = mat3 right up forward
  where forward = normalize $ target - pos
        right = normalize $ cross tup forward
        up = cross forward right

transform :: Mat3 -> Vec -> Vec
transform (Mat3 a b c d e f g h i) (Vec x y z) = Vec v1 v2 v3
  where v1 = a*x + b*y + c*z
        v2 = d*x + e*y + f*z
        v3 = g*x + h*y + i*z
