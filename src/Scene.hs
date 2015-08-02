module Scene (Object(..)
             , Scene(..)) where

import Geometry
import Material
import Light

data Object = Object Geometry Material

data Scene = Scene { shapes :: [Object], lights :: [Light] }
