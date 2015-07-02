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
  | (abs dDotn) > 0 = Just (Hit (rayAt ray t) n t color)
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

data Projection = Orthographic { width :: Float
                               , height :: Float} deriving Show

rayFromPixel :: Int -> Int -> Projection -> Int -> Int -> Ray
rayFromPixel w h (Orthographic pw ph) x y = Ray o (Vec 0 0 (-1)) where
  o = Vec (pw * (fx - (fw/2)) / fw)
      (ph * ((-fy) + (fh/2)) / fh) 0
  fw = fromIntegral w
  fh = fromIntegral h
  fx = fromIntegral x
  fy = fromIntegral y
  
diffuse :: Color -> Vec -> Vec -> Color
diffuse cd l n = C.mul (dot l n) cd
 
tracePixel :: [Shape] -> Int -> Int -> (Int, Int) -> Color
tracePixel scene w h (i, j) =
  let inter = closestIntersection scene ray
  in case inter of
    Just (Hit _ n _ color) -> diffuse color (normalize (Vec 1 1 1)) n
    Nothing -> gray 0.4
  where ray = (rayFromPixel w h
               (Orthographic 2.0 2.0)
               i j) 
          
rayTrace :: Int -> Int -> Image
rayTrace w h = generatePixels w h
               (tracePixel
                [Shape (Sphere (Vec  0.3    0.4  (-1)) 0.5) red,
                 Shape (Sphere (Vec (-0.4)  0.25   (-0.7)) 0.3) blue,
                 Shape (Sphere (Vec (-0.3) (-0.4) (-1)) 0.2) green,
                 Shape (Plane (Vec 0 0 (-2)) zAxis) red]
                w h)

main :: IO ()
main = do
  let output = rayTrace 256 256
  writePPM "out.ppm" output
  putStrLn "done!"

