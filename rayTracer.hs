{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.List
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import System.Environment (getArgs)

import Math
import Color
import Image
import Vec hiding (o)

import qualified Vec (o)

{- Raytracing -}
data Shape = Sphere { center :: Vec
                    , radius :: Float } deriving Show
data Ray = Ray { origin :: Vec
               , direction :: Vec} deriving Show

rayAt :: Ray -> Float -> Vec
rayAt (Ray o d) t = o + mul t d

intersects :: Ray -> Shape -> Bool
intersects (Ray o d) (Sphere ct r) = delta >= 0.0
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2

intersection :: Shape -> Ray -> Maybe Vec
intersection (Sphere ct r) ray@(Ray o d)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (rayAt ray t0)
  | otherwise = Just (rayAt ray t1) -- this is behind the origin
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2
        t0 = 0.5 * ((-b) - (sqrt delta))
        t1 = 0.5 * ((-b) + (sqrt delta))

rayFromPixel :: Int -> Int -> Int -> Int -> Ray
rayFromPixel w h x y = Ray o (Vec 0 0 (-1)) where
  o = Vec (fromIntegral (x - (w `div` 2)))
      (fromIntegral ((-y) + (h `div` 2))) 0

tracePixel :: Int -> Int -> (Int, Int) -> Color
tracePixel w h (i, j) =
  let inter = intersection (Sphere (Vec 50 50 (-100)) (fdiv h 4)) ray
  in case inter of
    Just p -> gray $ (dist p (origin ray) / 200)
    Nothing -> black
  where ray = (rayFromPixel w h i j) 
             
rayTrace :: Int -> Int -> Image
rayTrace w h = generatePixels w h (tracePixel w h)

main :: IO ()
main = do
  let output = rayTrace 384 256
  writePPM "out.ppm" output
  putStrLn "done!"

