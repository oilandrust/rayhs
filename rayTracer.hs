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
data Shape = Sphere { center :: Vec
                    , radius :: Float } deriving Show

normalAt :: Shape -> Vec -> Vec
normalAt (Sphere o r) v = normalize (v - o)

data Ray = Ray { origin :: Vec
               , direction :: Vec} deriving Show

data Hit = Hit { position :: Vec
               , normal :: Vec
               , t :: Float
               , color :: Color } deriving Show

rayAt :: Ray -> Float -> Vec
rayAt (Ray o d) t = o + Vec.mul t d

intersects :: Ray -> Shape  -> Bool
intersects (Ray o d) (Sphere ct r) = delta >= 0.0
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2

intersection :: Ray -> (Shape, Color) -> Maybe Hit
intersection ray@(Ray o d) (s@(Sphere ct r), color)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (Hit p0 n0 t0 color)
  | otherwise = Just (Hit p1 n1 t1 color)
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2
        t0 = 0.5 * ((-b) - (sqrt delta))
        t1 = 0.5 * ((-b) + (sqrt delta))
        p0 = rayAt ray t0
        p1 = rayAt ray t1
        n0 = normalAt s p0
        n1 = normalAt s p1

intersections :: [(Shape, Color)] -> Ray -> [Hit]
intersections scene ray = map fromJust
                          (filter isJust (map (intersection ray) scene))

closestIntersection :: [(Shape, Color)] -> Ray -> Maybe Hit
closestIntersection scene r = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` t) hits
  where hits = intersections scene r
        
rayFromPixel :: Int -> Int -> Int -> Int -> Ray
rayFromPixel w h x y = Ray o (Vec 0 0 (-1)) where
  o = Vec (fromIntegral (x - (w `div` 2)))
      (fromIntegral ((-y) + (h `div` 2))) 0

diffuse :: Color -> Vec -> Vec -> Color
diffuse cd l n = C.mul (dot l n) cd
 
tracePixel :: [(Shape, Color)] -> Int -> Int -> (Int, Int) -> Color
tracePixel scene w h (i, j) =
  let inter = closestIntersection scene ray
  in case inter of
    Just (Hit _ n _ color) -> diffuse color (normalize (Vec 1 1 1)) n
    Nothing -> gray 0.4
  where ray = (rayFromPixel w h i j) 
          
rayTrace :: Int -> Int -> Image
rayTrace w h = generatePixels w h
               (tracePixel [((Sphere (Vec   50    70  (-150)) (fdiv h 3)), red),
                            ((Sphere (Vec (-40)   25   (-80)) (fdiv h 6)), blue),
                            ((Sphere (Vec (-50) (-60) (-100)) (fdiv h 5)), green)
                           ]
                w h)

main :: IO ()
main = do
  let output = rayTrace 384 256
  writePPM "out.ppm" output
  putStrLn "done!"

