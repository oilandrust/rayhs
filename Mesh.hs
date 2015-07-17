module Mesh (Mesh(..)
            , readOBJ
            , translate) where

import Vec
import Geometry

import Data.List
import Data.Maybe
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))

import qualified Data.Vector as V

{- Triangle Mesh -}
data Mesh = Mesh { vertices :: Vector Vec
                 , normals :: Vector Vec
                 , indices :: [Int] } deriving Show

empty :: Mesh
empty = Mesh V.empty V.empty []

instance Inter Mesh where
  intersection = rayInterMesh

{- Ray Intersection -}
rayInterMesh :: Ray -> Mesh -> Maybe Hit
rayInterMesh ray mesh@(Mesh pts _ ids) = do
  let hits = catMaybes $ mapTriangles (triangleIntersection ray) pts ids
    in case hits of
    [] -> Nothing
    xs -> Just $ minimumBy (compare `on` t) xs

triangleIntersection :: Ray -> (Vec, Vec, Vec) -> Maybe Hit
triangleIntersection ray@(Ray o d) (p0, p1, p2) = do
  case (abs(det) < eps ||
        u < 0 || u > 1 ||
        v < 0 || (u + v) > 1 ||
        t < eps) of
    True -> Nothing
    False -> Just $ Hit (rayAt ray t) n t 
  where e1 = p1 - p0
        e2 = p2 - p0
        p = cross d e2
        det = dot e1 p
        idet = 1 / det
        t0 = o - p0
        u = idet * (dot t0 p)
        q = cross t0 e1
        v = idet * (dot d q)
        t = idet * (dot e2 q)
        n = normalize (cross (p1 - p0) (p2 - p0))

translate :: Mesh -> Vec -> Mesh
translate (Mesh v n i) t = Mesh (V.map (+t) v) n i

mapTriangles :: ((Vec, Vec, Vec) -> b) -> Vector Vec -> [Int] -> [b]
mapTriangles f pts indices = map f
                             (map (\(i, j, k) -> (pts ! i, pts ! j, pts ! k))
                              (faces indices))
  where faces [] = []
        faces (i:j:k:is) = (i, j, k):(faces is)


{- Obj Loading -}

data OBJToken = Vert Vec | Norm Vec | Face [Int] deriving Show

readVertex :: String -> Maybe OBJToken
readVertex s = let floats = readDoubles (words s) in
  case (length floats) of
    3 -> Just $ (Vert $ fromList floats)
    _ -> Nothing

readFace :: String -> Maybe OBJToken
readFace s = let ints = readInts (words s) in
  case (length ints) of
    0 -> Nothing
    _ -> Just . Face $ map (+(-1)) ( ints)
    where orient (x:y:xs) = (y:x:xs)
          orient xs = xs

parseLine :: String -> Maybe OBJToken
parseLine ('v':' ':rest) = readVertex rest
parseLine ('f':' ':rest) = readFace rest
parseLine _ = Nothing

buildMesh :: [OBJToken] -> Mesh
buildMesh tokens = Mesh verts norms inds where
  (verts, norms, inds) = foldl dispatch (V.empty, V.empty, []) tokens
  dispatch (vs, ns, is) (Vert v) = (V.snoc vs v, ns, is)
  dispatch (vs, ns, is) (Norm n) = (vs, V.snoc ns n, is) 
  dispatch (vs, ns, is) (Face f) = (vs, ns, is ++ f)

readOBJ :: String -> IO (Mesh)
readOBJ path = do
  content <- readFile path
  return . buildMesh . catMaybes $ map parseLine (lines content)

{- helper functions -}
readDoubles :: [String] -> [Double]
readDoubles = map read

readInts :: [String] -> [Int]
readInts = map read

fromList :: [Double] -> Vec
fromList d = Vec (d !! 0) (d !! 1) (d !! 2)
