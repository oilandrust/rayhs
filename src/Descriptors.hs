module Descriptors (GeometryDesc(..)
                   , ObjectDesc(..)
                   , SceneDesc(..)
                   , RenderDesc(..)
                   , buildScene) where

import Vec
import Light
import MaterialDescriptors
import Geometry
import Projection
import Mesh hiding (transform)
import qualified Mesh
import Scene
import KDTree
import Transform hiding (transform)

{- Scene Descriptor -}
data RenderDesc = RenderDesc { scene :: SceneDesc
                             , camera :: Camera
                             , width :: Int
                             , height :: Int
                             , maxDepth :: Int }

data GeometryDesc = MeshDesc { fileName :: String
                             , transform :: Transform }
                  | SphereDesc { center :: Vec
                               , radius :: Double }
                  | PlaneDesc { normal :: Vec
                              , point :: Vec
                              , tangent :: Vec } deriving Show

data ObjectDesc = ObjectDesc { geometry :: GeometryDesc
                             , material :: MaterialDesc } deriving Show

data SceneDesc = SceneDesc { objects :: [ObjectDesc]
                           , lights :: [Light] } deriving Show

buildScene :: SceneDesc -> IO Scene
buildScene (SceneDesc objectDescs lights) = do
  objects <- mapM buildObject objectDescs
  return $ Scene objects lights

buildObject :: ObjectDesc -> IO Object
buildObject (ObjectDesc objDesc matDesc) = do
  geometry <- buildGeometry objDesc
  material <- buildMaterial matDesc
  return $ Object geometry material

buildGeometry :: GeometryDesc -> IO Geometry
buildGeometry (PlaneDesc p n t) = return (geom $ Plane p n t)
buildGeometry (SphereDesc c r) = return (geom $ Sphere c r)
buildGeometry (MeshDesc filename trans) = do
  mesh <- readOBJ filename
  return (geom $ buildKDTree (Mesh.transform mesh trans))
