module KDTree () where

import Math
import Vec
import Geometry
import Mesh

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

{- Bounding Box -}
data Box = Box Vec Vec deriving Show

empty :: Box
empty = Box (Vec inf inf inf) (Vec (-inf) (-inf) (-inf))

include :: Box -> Vec -> Box
include (Box min max) p = Box (minV min p) (maxV  max p) 

buildBoundingBox :: Mesh -> Box
buildBoundingBox mesh = V.foldl (\box (p, _, _) -> include box p)
                        empty (vertices mesh) 

{- KDTree -}
type Triangle = (Vec, Vec, Vec)

data KDNode = Leaf Box (Vector Triangle)
            | Node Box KDNode KDNode

{-

{- Intersection -}
rayInter :: Ray -> KDTree -> Maybe Hit
rayInter ray (KDTree plane left right)
rayInter ray (Leaf triangles)


{- Construction -}
makeKDTree :: [Mesh] -> KDTree
makeKDTree meshes =

-}
