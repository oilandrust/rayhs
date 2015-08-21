module Projection (Projection(..)
                  , Camera(..)
                  , rayFromPixel ) where

import Vec
import Transform
import Geometry (Ray(..))

{- Projection -}
data Projection = Orthographic { width :: Double, height :: Double}
                | Perspective { fovy :: Double
                              , width :: Double
                              , height :: Double
                              , near :: Double }
                  deriving Show

data Camera = LookAt { position :: Vec
                     , target :: Vec
                     , up :: Vec
                     , projection :: Projection } deriving Show

rayFromPixel :: Double -> Double -> Camera -> Double -> Double -> Ray
rayFromPixel w h (LookAt p t up proj) px py = Ray (o+p) (transform mat d)
  where (Ray o d) = unproject w h proj px py
        mat = lookAt p t up

{- Create ray from camera givent the projection -}
unproject :: Double -> Double -> Projection
             -> Double -> Double -> Ray
unproject w h (Orthographic pw ph) px py = Ray o (Vec 0 0 1)
  where (apw, aph) = aspectSize w h
        o = Vec (apw * (px - (w/2)) / w) (aph * ((-py) + (h/2)) / h) 0

unproject w h (Perspective fovy pw ph n) px py = Ray o d
  where f = 0.5 * h / (tan 0.5 * fovy)
        (apw, aph) = aspectSize w h
        viewPlanePos = Vec (apw * (px - (w/2)) / w) (aph * ((-py) + (h/2)) / h) f
        d = normalize viewPlanePos
        o = Vec.o --Vec ((x viewPlanePos) * n / f) ((y viewPlanePos) * n / f) n

aspectSize :: Double -> Double -> (Double, Double)
aspectSize w h = (apw, aph)
  where aspect = w / h
        (apw, aph) = if aspect > 1
                     then (w, w/aspect)
                     else (aspect*h, h)
