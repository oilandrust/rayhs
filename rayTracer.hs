{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.Maybe
import Data.List
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import System.Environment (getArgs)

import Math
import Color hiding (mul)
import Image
import Vec hiding (o, mul)

import qualified Vec (o, mul)
import qualified Color as C (mul)

{- Raytracing -}
data Geometry = Sphere { center :: Vec, radius :: Float }
              | Plane Vec Vec
              deriving Show
                                                    
data Shape = Shape Geometry Color deriving Show

data Light = Directional { dir :: Vec, col :: Color } deriving Show

normalAt :: Geometry -> Vec -> Vec
normalAt (Sphere o r) v = normalize (v - o)
normalAt (Plane p n) _ = n

data Ray = Ray { origin :: Vec
               , direction :: Vec} deriving Show

data Hit = Hit { position :: Vec
               , normal :: Vec
               , t :: Float
               , color :: Color } deriving Show

rayAt :: Ray -> Float -> Vec
rayAt (Ray o d) t = o + Vec.mul t d

intersection :: Ray -> Shape -> Maybe Hit
intersection ray@(Ray o d) (Shape s@(Sphere ct r) color)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (Hit p0 n0 t0 color)
  | otherwise = Nothing
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2
        t0 = 0.5 * ((-b) - (sqrt delta))
        p0 = rayAt ray t0
        n0 = normalAt s p0

intersection ray@(Ray o d) (Shape (Plane p n) color)
  | (abs dDotn) > 0 && t > 0 = Just (Hit (rayAt ray t) n t color)
  | otherwise = Nothing
    where dDotn = dot d n
          t = dot n (p - o) / dDotn

intersections :: [Shape] -> Ray -> [Hit]
intersections scene ray = map fromJust
                          (filter isJust (map (intersection ray) scene))

closestIntersection :: [Shape] -> Ray -> Maybe Hit
closestIntersection scene r = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` t) hits
  where hits = intersections scene r

data Projection = Orthographic { width :: Float, height :: Float}
                | Perspective { fovy :: Float
                              , width :: Float
                              , height :: Float
                              , near :: Float }
                  deriving Show

rayFromPixel :: Float -> Float -> Projection -> Float -> Float -> Ray
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
  
diffuse :: Color -> Color -> Vec -> Vec -> Color
diffuse cd lc l n = C.mul ((max (dot l n) 0) * piInv) (cd * lc)

accumDiffuse :: [Shape] -> [Light] -> Vec -> Vec -> Color -> Color
accumDiffuse sc lts p n color =
  foldl (\c l -> c + (diffFromLight l)) black lts
  where diffFromLight light =
          let inter = closestIntersection sc (Ray (p+(Vec.mul 0.0001 ld)) ld)
          in case inter of
            Just _ -> black
            Nothing -> (diffuse color (col light) ld n)
          where ld = dir light
        
traceRay :: [Shape] -> [Light] -> Ray -> Color
traceRay shapes lights ray =
  let inter = closestIntersection shapes ray
  in case inter of
    Just (Hit p n _ c) -> 
      (C.mul 0.2 c) + (accumDiffuse shapes lights p n c)
    Nothing -> black
                    
tracePixel :: [Shape]-> [Light] -> Float -> Float -> (Float, Float) -> Color
tracePixel scene lights w h (i, j) = traceRay scene lights ray
  where ray = (rayFromPixel w h
               (Perspective 0 2 2 0.1)
               i j)

rayTrace :: Int -> Int -> Image
rayTrace w h = generatePixels w h
               (tracePixel
                [Shape (Sphere (Vec 0.3 (-0.5) 1.5) 0.5) red,
                 Shape (Sphere (Vec (-0.4) 0.25 0.7) 0.3) blue,
                 Shape (Sphere (Vec (-0.3) (-0.4) 1) 0.2) green,
                 --Shape (Plane (Vec 0 0 2) (-zAxis)) white,
                 --Shape (Plane (Vec 1 0 0) (-xAxis)) green,
                 --Shape (Plane (Vec (-1) 0 0) (xAxis)) red,
                 --Shape (Plane (Vec 0 1 0) (-yAxis)) white,
                 Shape (Plane (Vec 0 (-1) 0) yAxis) white]
                [Directional (normalize (Vec 1 0.7 (-1))) (gray 1.2),
                 Directional (normalize (Vec (-0.7) 1 (-1))) (RGB 0.8 0.8 0.7)]
                (fromIntegral w)
                (fromIntegral h))

main :: IO ()
main = do
  let output = rayTrace 512 512
  writePPM "out.ppm" output
  putStrLn "done!"

