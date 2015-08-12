
module JSON ( encode
            , decode
            , FromJSON(..)
            ) where

import Data.String
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Control.Applicative

import Descriptors
import Vec
import Color
import Material
import Light

instance FromJSON Vec where
  parseJSON (Object v) = Vec <$>
                         v .: "x" <*>
                         v .: "y" <*>
                         v .: "z"
  parseJSON _ = mempty

instance FromJSON Color where
  parseJSON (Object v) = RGB <$>
                         v .: "r" <*>
                         v .: "g" <*>
                         v .: "b"
  parseJSON _ = mempty


typeName :: Text
typeName = "type"

instance FromJSON ColorMap where
  parseJSON = withObject "colorMap" $ \o -> do
    kind <- o .: typeName
    case kind of
      "flat" -> Flat <$> o .: "color"
      "checker" -> CheckerBoard <$>
                   o .: "color1" <*>
                   o .: "color2" <*>
                   o .: "size"
      _ -> fail ("Unknown type for color map " ++ kind)

instance FromJSON Material where
  parseJSON = withObject "material" $ \obj -> do
    kind <- obj .: typeName
    case kind of
      "mirror" -> Mirror <$> obj .: "ior"
      "diffuse" -> Diffuse <$> obj .: "cd"
      "plastic" -> Plastic <$> obj .: "cd" <*> obj .: "ior"
      "emmit" -> Emmit <$> obj .: "ce"
      "transparent" -> Transparent <$> obj .: "ior"
      _ -> fail ("Unknown type for material " ++ kind)

instance FromJSON Light where
  parseJSON = withObject "light" $ \obj -> do
    kind <- obj .: typeName
    case kind of
      "directional" -> Directional <$> obj .: "direction" <*> obj .: "color"
      "point" -> Point <$>
                 obj .: "position" <*>
                 obj .: "color" <*>
                 obj .: "radius"
      _ -> fail ("Unknown type for light " ++ kind)

instance FromJSON GeometryDesc where
  parseJSON = withObject "geometry" $ \obj -> do
    kind <- ((obj .: typeName) :: Parser String)
    case kind of
      "sphere" -> SphereDesc <$>
                  obj .: "center" <*>
                  obj .: "radius"
      "plane" -> PlaneDesc <$>
                 obj .: "normal" <*>
                 obj .: "point" <*>
                 obj .: "tangent"
      "mesh" -> MeshDesc <$>
                obj .: "fileName" <*>
                obj .: "translation"

instance FromJSON ObjectDesc where
  parseJSON (Object v) = ObjectDesc <$>
                         v .: "geometry" <*>
                         v .: "material"
  parseJSON _ = mempty

instance FromJSON SceneDesc where
  parseJSON (Object v) = SceneDesc <$>
                         v .: "objects" <*>
                         v .: "lights"
  parseJSON _ = mempty
