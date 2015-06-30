module Color (Color(RGB)
              , black
              , red
              , green
              , blue
              , gray) where

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

gray :: Float -> Color
gray g = RGB g g g
