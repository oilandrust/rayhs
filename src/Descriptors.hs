module Descriptors (GeometryDesc(..)
                   , ObjectDesc(..)
                   , SceneDesc(..)
                   , RenderDesc(..)
                   , buildScene) where

import Vec
import Light
import Material
import Geometry
import Projection
import Mesh
import Scene
import KDTree

{- Scene Descriptor -}
data RenderDesc = RenderDesc { scene :: SceneDesc
                             , camera :: Camera
                             , width :: Int
                             , height :: Int
                             , maxDepth :: Int }

data GeometryDesc = MeshDesc { fileName :: String
                             , translation :: Vec }
                  | SphereDesc { center :: Vec
                               , radius :: Double }
                  | PlaneDesc { normal :: Vec
                              , point :: Vec
                              , tangent :: Vec } deriving Show

data ObjectDesc = ObjectDesc { geometry :: GeometryDesc
                             , material :: Material } deriving Show

data SceneDesc = SceneDesc { objects :: [ObjectDesc]
                           , lights :: [Light] } deriving Show

buildScene :: SceneDesc -> IO Scene
buildScene (SceneDesc objectDescs lights) = do
  objects <- mapM buildObject objectDescs
  return $ Scene objects lights

buildObject :: ObjectDesc -> IO Object
buildObject (ObjectDesc desc mat) = do
  geometry <- buildGeometry desc
  return $ Object geometry mat

buildGeometry :: GeometryDesc -> IO Geometry
buildGeometry (PlaneDesc p n t) = return (geom $ Plane p n t)
buildGeometry (SphereDesc c r) = return (geom $ Sphere c r)
buildGeometry (MeshDesc filename pos) = do
  mesh <- readOBJ filename
  return (geom $ buildKDTree (translate mesh pos))
