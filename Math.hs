module Math (fdiv
            , piInv) where

import Data.Function

fdiv :: Int -> Int -> Float
fdiv = (/) `on` fromIntegral

piInv :: Float
piInv = 1 / pi
