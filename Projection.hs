module Projection (Projection(..)
                  , rayFromPixel ) where

import Vec
import Geometry (Ray(..))

{- Projection -}
data Projection = Orthographic { width :: Double, height :: Double}
                | Perspective { fovy :: Double
                              , width :: Double
                              , height :: Double
                              , near :: Double }
                  deriving Show

{- Create ray from camera givent the projection -}
rayFromPixel :: Double -> Double -> Projection -> Double -> Double -> Ray
rayFromPixel w h (Orthographic pw ph) x y = Ray o (Vec 0 0 1) where
  o = Vec (apw * (x - (w/2)) / w) (aph * ((-y) + (h/2)) / h) 0
  aspect = w / h
  (apw, aph) = case (aspect>1) of
    True -> (pw, pw/aspect)
    False -> (aspect*ph, ph)

rayFromPixel w h (Perspective f pw ph n) x y = Ray o d where
  o = Vec (apw * (x - (w/2)) / w) (aph * ((-y) + (h/2)) / h) 0
  d = normalize $ (o - (Vec 0 0 (-2)))
  aspect = w / h
  (apw, aph) = case (aspect>1) of
    True -> (pw, pw/aspect)
    False -> (aspect*ph, ph)