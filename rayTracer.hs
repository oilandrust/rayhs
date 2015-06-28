{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import System.Environment (getArgs)


data Color = RGB Float Float Float deriving Show

data Image = Image { width  :: Int
                   , height :: Int
                   , pixels :: Vector Color }

black :: Color
black = RGB 0 0 0
red :: Color
red = RGB 1 0 0
green :: Color
green = RGB 0 1 0
blue :: Color
blue = RGB 0 0 1

solidImage :: Int -> Int -> Color -> Image
solidImage w h c = Image w h (V.replicate (w*h) c)

emptyImage :: Int -> Int -> Image
emptyImage w h = solidImage w h black

toIntC :: Float -> Int
toIntC c = truncate (255 * c)

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

rayTrace :: Int -> Int -> Image
rayTrace w h = solidImage w h red

main :: IO ()
main = do
  args <- getArgs
  let output = rayTrace 256 256
  writePPM "out.ppm" output
  putStrLn "done!"

