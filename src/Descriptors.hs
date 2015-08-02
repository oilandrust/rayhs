module Descriptors (GeometryDesc(..)
                   , ObjectDesc
                   , SceneDesc
                   , buildScene) where

import Vec
import Light
import Material
import Geometry
import Mesh
import Scene
import KDTree

{- Scene Descriptor -}
data GeometryDesc = MeshDesc String Vec
                  | SphereDesc Vec Double
                  | PlaneDesc Vec Vec Vec

type ObjectDesc = (GeometryDesc, Material)

type SceneDesc = ([ObjectDesc], [Light])

buildScene :: SceneDesc -> IO Scene
buildScene (objectDescs, lights) = do
  objects <- mapM buildObject objectDescs
  return $ Scene objects lights

buildObject :: ObjectDesc -> IO Object
buildObject (desc, mat) = do
  geometry <- buildGeometry desc
  return $ Object geometry mat

buildGeometry :: GeometryDesc -> IO Geometry
buildGeometry (PlaneDesc p n t) = return (geom $ Plane p n t)
buildGeometry (SphereDesc c r) = return (geom $ Sphere c r)
buildGeometry (MeshDesc filename pos) = do
  mesh <- readOBJ filename
  return (geom $ buildKDTree (translate mesh pos))
