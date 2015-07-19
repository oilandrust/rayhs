module Math (fdiv
            , piInv
            , twoPiInv) where

import Data.Function

fdiv :: Int -> Int -> Double
fdiv = (/) `on` fromIntegral

piInv :: Double
piInv = 1 / pi

twoPiInv :: Double
twoPiInv = 0.5 / pi
