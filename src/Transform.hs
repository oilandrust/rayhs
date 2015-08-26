module Transform (Transform(..)
                 , transform) where

import Vec
import Mat

data Transform = Translate Vec
               | Scale Vec
               | Rotate Vec Double
               | RotateX Double
               | RotateY Double
               | RotateZ Double
               | Sequence [Transform] deriving Show


transform :: Transform -> Vec -> Vec
transform (Translate t) p = p + t
transform (Scale (Vec sx sy sz)) (Vec x y z) = Vec (sx*x) (sy*y) (sz*z)
transform (Rotate axis angle) v = apply rMat v
  where rMat = rotate axis angle
transform (RotateX angle) p = apply (rotateX angle) p
transform (RotateY angle) p = apply (rotateY angle) p
transform (RotateZ angle) p = apply (rotateZ angle) p
transform (Sequence transforms) p = foldl (flip transform) p transforms
