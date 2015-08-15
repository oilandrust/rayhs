{-# LANGUAGE OverloadedStrings #-}
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
import Projection

instance FromJSON Vec where
  parseJSON (Object v) = Vec <$>
                         v .: "x" <*>
                         v .: "y" <*>
                         v .: "z"
  parseJSON _ = fail "Error reaging Vec"

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

instance FromJSON Projection where
  parseJSON = withObject "projection" $ \obj -> do
    kind <- obj .: typeName
    case kind of
      "orthographic" -> Orthographic <$> obj .: "width" <*> obj .: "height"
      "perspective" -> Perspective <$>
                 obj .: "fovy" <*>
                 obj .: "width" <*>
                 obj .: "height" <*>
                 obj .: "near"
      _ -> fail ("Unknown type for projection " ++ kind)

instance FromJSON Camera where
  parseJSON (Object v) = LookAt <$>
                         v .: "position" <*>
                         v .: "target" <*>
                         v .: "up" <*>
                         v .: "projection"
  parseJSON _ = mempty


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

instance FromJSON RenderDesc where
  parseJSON (Object v) = RenderDesc <$>
                         v .: "scene" <*>
                         v .: "camera" <*>
                         v .: "width" <*>
                         v .: "height" <*>
                         v .: "maxDepth"

{- encoding -}

instance ToJSON Vec where
  toJSON (Vec x y z) = object ["x" .= x, "y" .= y, "z" .= z]

instance ToJSON Color where
  toJSON (RGB x y z) = object ["r" .= x, "g" .= y, "b" .= z]

instance ToJSON ColorMap where
  toJSON (Flat c) = object ["type" .= ("flat" :: Text), "color" .= c]
  toJSON (CheckerBoard c1 c2 s) = object ["type" .= ("checker" :: Text),
                                          "color2" .= c2,
                                          "color1" .= c1,
                                          "size" .= s]

instance ToJSON Material where
  toJSON (Diffuse cd) = object ["type" .= ("diffuse" :: Text), "cd" .= cd]
  toJSON (Emmit cd) = object ["type" .= ("emmit" :: Text), "ce" .= cd]
  toJSON (Plastic cd ior) = object ["type" .= ("plastic" :: Text),
                                    "cd" .= cd,
                                    "ior" .= ior]
  toJSON (Mirror ior) = object ["type" .= ("mirror" :: Text),
                                "ior" .= ior]
  toJSON (Transparent ior) = object ["type" .= ("transparent" :: Text),
                                     "ior" .= ior]

instance ToJSON Light where
  toJSON (Directional d c) = object ["type" .= ("directional" :: Text),
                                     "direction" .= d,
                                     "color" .= c]
  toJSON (Point pos color rad) = object ["type" .= ("point" :: Text),
                                         "position" .= pos,
                                         "color" .= color,
                                         "radius" .= rad]

instance ToJSON GeometryDesc where
  toJSON (SphereDesc c r) = object ["type" .= ("sphere" :: Text),
                                    "center" .= c,
                                    "radius" .= r]
  toJSON (PlaneDesc n p t) = object ["type" .= ("plane" :: Text),
                                    "normal" .= n,
                                    "point" .= p,
                                    "tangent" .= t]
  toJSON (MeshDesc fn t) = object ["type" .= ("mesh" :: Text),
                                   "fileName" .= fn,
                                   "translation" .= t]

instance ToJSON ObjectDesc where
  toJSON (ObjectDesc g m) = object ["material" .= m,
                                    "geometry" .= g]

instance ToJSON SceneDesc where
  toJSON (SceneDesc objs lights) = object ["objects" .= objs,
                                           "lights" .= lights]
