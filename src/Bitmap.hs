module Bitmap (Bitmap(..)
              , (@@)
              , readPPM) where

import Data.List
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import Math
import Color

{- Image -}
data Bitmap = Bitmap { width  :: !Int
                     , height :: !Int
                     , pixels :: Vector Color } deriving Show

(@@) :: Bitmap -> (Int, Int)  -> Color
(Bitmap w _ pixels) @@ (i, j) = pixels ! (i + w * j)

readWidthAndHeight :: String -> (Int, Int)
readWidthAndHeight s = (head ints, ints !! 1)
  where ints = map read (words s)

colors :: [Double] -> [Color]
colors (r:g:b:rest) = (RGB r g b:colors rest)
colors _ = []

readPixels :: [String] -> [Color]
readPixels words = colors $ map (\s -> (read s) / 255) words

readPPM :: String -> IO Bitmap
readPPM path = do
  content <- readFile path
  let (header, pixels) = splitAt 3 (lines content)
  let (w, h) = readWidthAndHeight $ header !! 1
  let maxC = (read $ header !! 2) :: Int
  return $ Bitmap w h (V.fromList $ readPixels $ concat $ map words pixels)
