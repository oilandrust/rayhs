module Math (fdiv) where

import Data.Function

fdiv :: Int -> Int -> Float
fdiv = (/) `on` fromIntegral
