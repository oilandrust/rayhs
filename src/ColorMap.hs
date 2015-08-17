module ColorMap (ColorMap(..)
                , colorAt) where

import Data.Fixed

import Color
import Math
import Vec
import Bitmap

data ColorMap = Flat { color :: !Color }
              | CheckerBoard { color1 :: !Color
                             , color2 :: !Color
                             , size :: !Double }
              | Texture { image :: !Bitmap }
              deriving Show

colorAt :: ColorMap -> UV -> Color
{-# INLINE colorAt #-}
colorAt (Flat color) _ = color
colorAt (CheckerBoard c0 c1 scale) (UV u v) =
  if (mod' u scale - (0.5*scale)) * (mod' v scale - (0.5*scale)) < 0
  then c0
  else c1

colorAt (Texture bitmap@(Bitmap w h pixels)) uv = bilinearInterp c0 c1 c2 c3 lx ly
  where (u, v) = toPixel w h uv
        ui = round u
        vi = round v
        x0 = (ui-1) `mod` w
        x1 = ui `mod` w
        y0 = (vi-1) `mod` h
        y1 = vi `mod` h
        lx = u - (fromIntegral $ ui - 1) - 0.5
        ly = v - (fromIntegral $ vi - 1) - 0.5
        c0 = bitmap Bitmap.@@ (x0, y0)
        c1 = bitmap Bitmap.@@ (x1, y0)
        c2 = bitmap Bitmap.@@ (x0, y1)
        c3 = bitmap Bitmap.@@ (x1, y1)

bilinearInterp :: Color -> Color -> Color -> Color -> Double -> Double -> Color
{-# INLINE bilinearInterp #-}
bilinearInterp c0 c1 c2 c3 u v = mul v cx1 + mul (1-v) cx0
  where cx0 = mul u c1 + mul (1-u) c0
        cx1 = mul u c3 + mul (1-u) c2

toPixel :: Int -> Int -> UV -> (Double, Double)
{-# INLINE toPixel #-}
toPixel w h uv = (ui, vi)
  where (UV u v) = repeatUV uv
        ui = u * (fromIntegral w)
        vi = v * (fromIntegral h)

repeatUV :: UV -> UV
{-# INLINE repeatUV #-}
repeatUV (UV u v) = UV ui vi
  where ui = mod' u 1
        vi = mod' v 1
