{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.List
import Data.Function
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import System.Environment (getArgs)

{- Color -}
data Color = RGB Float Float Float deriving Show

black :: Color
black = RGB 0 0 0
red :: Color
red = RGB 1 0 0
green :: Color
green = RGB 0 1 0
blue :: Color
blue = RGB 0 0 1

toIntC :: Float -> Int
toIntC c = truncate (255 * c)

fdiv :: Int -> Int -> Float
fdiv = (/) `on` fromIntegral

{- Raytracing -}
data Vec = Vec Float Float Float deriving (Eq, Show)
instance Num Vec where
  (Vec x1 y1 z1) + (Vec x2 y2 z2) = (Vec (x1+x2) (y1+y2) (z1+z2))
  (Vec x1 y1 z1) * (Vec x2 y2 z2) = (Vec (x1*x2) (y1*y2) (z1*z2))
  (Vec x1 y1 z1) - (Vec x2 y2 z2) = (Vec (x1-x2) (y1-y2) (z1-z2))
  negate (Vec x y z) = (Vec (-x) (-y) (-z))  

data Shape = Sphere { center :: Vec
                    , radius :: Float } deriving Show
data Ray = Ray { origin :: Vec
               , direction :: Vec} deriving Show

dot :: Vec -> Vec -> Float
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

sqrLen :: Vec -> Float
sqrLen v = dot v v

rayInterSphere :: Ray -> Shape -> Bool
rayInterSphere (Ray o d) (Sphere ct r) = delta > 0.0
  where delta = b^2 - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r^2

rayFromPixel :: Int -> Int -> Int -> Int -> Ray
rayFromPixel w h x y = Ray o (Vec 0 0 (-1)) where
  o = Vec (fromIntegral (x - (w `div` 2)))
      (fromIntegral (y - (h `div` 2))) 0

indices :: Int -> Int -> [(Int, Int)]
indices w h = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

mapPixels :: (Color -> (Int, Int) -> Color) -> Image -> Image
mapPixels f (Image w h px) = Image w h (V.zipWith f px
                                       (V.fromList (indices w h)))

rayTrace :: Int -> Int -> Image
rayTrace w h = mapPixels
               (\c (x, y) -> RGB (fdiv x w) (fdiv y h) 0)
               (emptyImage w h)
        
main :: IO ()
main = do
  args <- getArgs
  let output = rayTrace 256 256
  writePPM "out.ppm" output
  putStrLn "done!"

{- Image -}
data Image = Image { width  :: Int
                   , height :: Int
                   , pixels :: Vector Color }

solidImage :: Int -> Int -> Color -> Image
solidImage w h c = Image w h (V.replicate (w*h) c)

emptyImage :: Int -> Int -> Image
emptyImage w h = solidImage w h black

formatPixelsPPM :: Image -> String
formatPixelsPPM (Image w h pix) = concat $ intercalate ["\n"] 
                                  [[(formatPixel (pix ! (x+h*y)) ++ "  ")
                                   | x <- [0..w-1]]
                                   | y <- [0..h-1]]
  where formatPixel (RGB r g b) = (show $ toIntC r)
                                  ++ " " ++ (show $ toIntC g)
                                  ++ " " ++ (show $ toIntC b) 

writePPM :: String -> Image -> IO()
writePPM path image = do
  let content = "P3\n" ++ (show (width image))
                ++ " " ++ (show (height image)) ++ "\n255\n"
                ++ (formatPixelsPPM image)
  writeFile path content
