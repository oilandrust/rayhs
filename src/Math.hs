module Math (fdiv
            , piInv
            , twoPiInv
            , inf) where

import Data.Function

fdiv :: Int -> Int -> Double
fdiv = (/) `on` fromIntegral

piInv :: Double
piInv = 1 / pi

twoPiInv :: Double
twoPiInv = 0.5 / pi

inf :: Double
inf = read "Infinity"
