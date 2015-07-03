module Image (Image(Image)
             , generatePixels
             , mapPixels
             , gradient
             , solidImage
             , emptyImage
             , writePPM) where

import Data.List
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import Math
import Color

{- Image -}
data Image = Image { width  :: Int
                   , height :: Int
                   , pixels :: Vector Color } deriving Show

mapPixels :: (Color -> (Int, Int) -> Color) -> Image -> Image
mapPixels f (Image w h px) = Image w h (V.zipWith f px
                                        (V.fromList indices))
  where indices = [(x,y) | y <- [0..h-1], x <- [0..w-1]] 

generatePixels :: Int -> Int -> ((Float, Float) -> Color) -> Image
generatePixels w h f = Image w h (V.generate (w*h)
                                  (\i -> f ((fromIntegral (i `mod` w)),
                                            (fromIntegral (i `div` w)))))

gradient :: Int -> Int -> Image
gradient w h = Image w h (V.generate (w*h)
                         (\i -> gray (fdiv i (w * h - 1))))

solidImage :: Int -> Int -> Color -> Image
solidImage w h c = Image w h (V.replicate (w*h) c)

emptyImage :: Int -> Int -> Image
emptyImage w h = solidImage w h black

toIntC :: Float -> Int
toIntC c = truncate (255 * (min c 1))

formatPixelsPPM :: Image -> String
formatPixelsPPM (Image w h pix) = concat $ intercalate ["\n"] 
                                  [[(formatPixel (pix ! (x+w*y)) ++ "  ")
                                   | x <- [0..(w-1)]]
                                   | y <- [0..(h-1)]]
  where formatPixel (RGB r g b) = (show $ toIntC r)
                                  ++ " " ++ (show $ toIntC g)
                                  ++ " " ++ (show $ toIntC b) 

writePPM :: String -> Image -> IO()
writePPM path image = do
  let content = "P3\n" ++ (show (width image))
                ++ " " ++ (show (height image)) ++ "\n255\n"
                ++ (formatPixelsPPM image)
  writeFile path content
