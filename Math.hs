module Math (fdiv
            , piInv) where

import Data.Function

fdiv :: Int -> Int -> Double
fdiv = (/) `on` fromIntegral

piInv :: Double
piInv = 1 / pi
