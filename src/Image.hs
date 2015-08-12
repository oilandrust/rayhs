module Image (Image(Image)
             , generatePixels
             , generatePixelsRnd
             , mapPixels
             , gradient
             , solidImage
             , emptyImage
             , writePPM) where

import Data.List
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import Control.Parallel.Strategies

import Math
import Color
import RandomSamples

{- Image -}
data Image = Image { width  :: Int
                   , height :: Int
                   , pixels :: [Color] } deriving Show

mapPixels :: (Color -> (Int, Int) -> Color) -> Image -> Image
mapPixels f (Image w h px) = Image w h (zipWith f px indices)
  where indices = [(x,y) | y <- [0..h-1], x <- [0..w-1]]

instance NFData Color

pixelCoord :: Int -> Int -> Int -> (Double, Double)
pixelCoord w h i = (fromIntegral (i `mod` w), fromIntegral (i `div` w))

generatePixels :: Int -> Int -> ((Double, Double) -> Color) -> Image
generatePixels w h f = Image w h (map (f . pixelCoord w h) [0..(w*h)]
                                  `using` parListChunk 10 rdeepseq )

generatePixelsRnd :: Int -> Int -> ((Double, Double) -> Rnd Color) -> Rnd Image
generatePixelsRnd w h f = do
  pixels <- mapM (f . pixelCoord w h) [0..(w*h)]
  return $ Image w h pixels

gradient :: Int -> Int -> Image
gradient w h = Image w h (map
                         (\i -> gray (fdiv i (w * h - 1)))
                         [0..(w*h)])

solidImage :: Int -> Int -> Color -> Image
solidImage w h c = Image w h (replicate (w*h) c)

emptyImage :: Int -> Int -> Image
emptyImage w h = solidImage w h black

toIntC :: Double -> Int
toIntC c = truncate $ 255 * min c 1

formatPixelsPPM :: Image -> String
formatPixelsPPM (Image w h pix) = concat $ intercalate ["\n"]
                                  [[formatPixel
                                     (V.fromList pix ! (x+w*y)) ++ "  "
                                   | x <- [0..(w-1)]]
                                   | y <- [0..(h-1)]]
  where formatPixel (RGB r g b) = show (toIntC r)
                                  ++ " " ++ show (toIntC g)
                                  ++ " " ++ show (toIntC b)

writePPM :: String -> Image -> IO()
writePPM path image = do
  let content = "P3\n" ++ show (width image)
                ++ " " ++ show (height image) ++ "\n255\n"
                ++ formatPixelsPPM image
  writeFile path content
