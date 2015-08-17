module MaterialDescriptors (MaterialDesc(..)
                            , ColorMapDesc(..)
                            , buildColorMap
                            , buildMaterial) where

import Material
import Color
import ColorMap
import Bitmap

data MaterialDesc = MirrorDesc { ior :: Double }
                  | DiffuseDesc { cd :: ColorMapDesc }
                  | PlasticDesc { cd :: ColorMapDesc, ior :: Double}
                  | EmmitDesc { ce :: Color }
                  | TransparentDesc { ior :: Double }
                  | ShowNormalDesc
                  | ShowUVDesc
                  deriving Show

data ColorMapDesc = FlatDesc { color :: Color }
                  | CheckerBoardDesc { color1 :: Color
                                     , color2 :: Color
                                     , size :: Double }
                  | TextureDesc { fileName :: String }
                  deriving Show

buildColorMap :: ColorMapDesc -> IO ColorMap
buildColorMap (FlatDesc c) = return $ Flat c
buildColorMap (CheckerBoardDesc c1 c2 s) = return $ CheckerBoard c1 c2 s
buildColorMap (TextureDesc fileName) = do
  image <- readPPM fileName
  return $ Texture image

buildMaterial :: MaterialDesc -> IO Material
buildMaterial (MirrorDesc ior) = return $ Mirror ior
buildMaterial (DiffuseDesc cdd) = do
  cd <- buildColorMap cdd
  return $ Diffuse cd
buildMaterial (PlasticDesc cdd ior) = do
  cd <- buildColorMap cdd
  return $ Plastic cd ior
buildMaterial (EmmitDesc ce) = return $ Emmit ce
buildMaterial (TransparentDesc ior) = return $ Transparent ior
buildMaterial ShowNormalDesc = return ShowNormal
buildMaterial ShowUVDesc = return ShowUV
