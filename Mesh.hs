module Mesh (Mesh(..)
            , readOBJ
            , translate) where

import Vec
import Geometry

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))
import Text.Read

import qualified Data.Vector as V

{- Triangle Mesh -}

{- As simple as possible indexed face set.
Represented by a list of positions, list of normals and list of indices.
It should be guaranted that there is as many normals as vertices.
Also, each index in the indices should be a valid index in the list of normals
and positions.
-}

type Vertex = (Position, Normal, UV)

data Mesh = Mesh { vertices :: Vector Vertex
                 , indices :: [Int] } deriving Show

empty :: Mesh
empty = Mesh V.empty []

instance Inter Mesh where
  intersection = rayInterMesh

{- Ray Intersection -}
rayInterMesh :: Ray -> Mesh -> Maybe Hit
rayInterMesh ray mesh = do
  let hits = catMaybes $ mapTriangles (triangleIntersection ray) mesh
    in case hits of
    [] -> Nothing
    xs -> Just $ minimumBy (compare `on` t) xs


-- Convenience type to store 3 vertices
type Triangle = (Vertex, Vertex, Vertex)

triLookup :: Vector Vertex -> (Int, Int, Int) -> Triangle
triLookup verts (i, j, k) = (verts ! i, verts ! j, verts ! k)

barycentricInterp :: (Num a, Ext a) =>
                     Double -> a -> Double -> a -> Double -> a
                     -> a
barycentricInterp a p b q c r = (mul a p) + (mul b q) + (mul c r)

triangleIntersection :: Ray -> Triangle -> Maybe Hit
triangleIntersection ray@(Ray o d) ((p0,n0,uv0), (p1,n1,uv1), (p2,n2,uv2)) = do
  case (abs(det) < eps ||
        u < 0 || u > 1 ||
        v < 0 || (u + v) > 1 ||
        t < eps) of
    True -> Nothing
    False -> Just $ Hit (rayAt ray t) n uv t 
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
        n = barycentricInterp u n1 v n2 (1-u-v) n0
        uv = barycentricInterp u uv1 v uv2 (1-u-v) uv0

translate :: Mesh -> Vec -> Mesh
translate mesh t = transformVertices (\(p,n,uv) -> (p+t,n,uv)) mesh

{- Mapping functions -}
triangles :: Mesh -> [Triangle]
triangles (Mesh vertices indices) =
  (map (\ids -> (triLookup vertices ids)) (faces indices))
  where faces [] = []
        faces (i:j:k:is) = (i, j, k):(faces is)

mapTriangles :: (Triangle -> b) -> Mesh -> [b]
mapTriangles f mesh = map f (triangles mesh)

transformVertices :: (Vertex -> Vertex) -> Mesh -> Mesh
transformVertices f mesh = Mesh (V.map f (vertices mesh)) (indices mesh)

{- Obj Loading -}

{-  Obj vertices can be Position, Normal or Texture Coord (not supported yet)
Faces must have a position index and have optionaly a normal index and
tc index
 -}

type OBJIndex = (Int, Maybe Int, Maybe Int)
data OBJToken = Vert Vec
              | Norm Vec
              | UVS UV
              | Face [OBJIndex]
              deriving Show

readDoubles :: [String] -> [Double]
readDoubles = map read

readVertex :: String -> Maybe OBJToken
readVertex s = let floats = readDoubles (words s) in
  case (length floats) of
    3 -> Just $ (Vert $ fromList floats)
    _ -> Nothing

readNormal :: String -> Maybe OBJToken
readNormal s = let floats = readDoubles (words s) in
  case (length floats) of
    3 -> Just $ (Norm $ fromList floats)
    _ -> Nothing

readUV :: String -> Maybe OBJToken
readUV s = let floats = readDoubles (words s) in
  case (length floats) of
    2 -> Just $ (UVS $ UV (floats !! 0) (floats !! 1))
    _ -> Nothing
    
readOBJIndex :: String -> Maybe OBJIndex
readOBJIndex s = parseIndex $ readMaybeInts s

readMaybeInts :: String -> [Maybe Int]
readMaybeInts s = map readMaybe (splitOn "/" s)

parseIndex :: [Maybe Int] -> Maybe OBJIndex 
parseIndex (Just pi:ti:ni:_) = Just (pi, ti, ni) 
parseIndex (Just pi:ti:_) = Just (pi, ti, Nothing)
parseIndex (Just pi:_) = Just (pi, Nothing, Nothing)
parseIndex _ = Nothing

readFace :: String -> Maybe OBJToken
readFace s = let indices = catMaybes $ map readOBJIndex (words s) in
  case ((length indices) > 2) of
    True -> Just $ Face indices
    False -> Nothing

readLine :: String -> Maybe OBJToken
readLine ('v':' ':rest) = readVertex rest
readLine ('v':'n':' ':rest) = readNormal rest
readLine ('v':'t':' ':rest) = readUV rest
readLine ('f':' ':rest) = readFace rest
readLine _ = Nothing

buildMeshData :: [OBJToken] -> ([Vec], [Vec], [UV], [OBJIndex])
buildMeshData tokens = foldl dispatch ([], [], [], []) tokens
 where dispatch (vs, ns, uvs, is) (Vert v) = (vs ++ [v], ns, uvs, is)
       dispatch (vs, ns, uvs, is) (Norm n) = (vs, ns ++ [n], uvs, is)
       dispatch (vs, ns, uvs, is) (UVS u) = (vs, ns, uvs ++ [u], is) 
       dispatch (vs, ns, uvs, is) (Face f) = (vs, ns, uvs, is ++ f)

-- Functional style lookup table
type IndexMap = OBJIndex -> Maybe Int
emptyMap :: IndexMap
emptyMap _ = Nothing

insertIndex :: IndexMap -> OBJIndex -> Int -> IndexMap
insertIndex imap oi i query = case query == oi of
  True -> Just i
  False -> imap query

flattenVertices :: ([Position], [Normal], [UV], [OBJIndex]) -> ([Vertex], [Int])
flattenVertices (pos, norms, uvs, inds) = (verts, ids)
  where (verts, ids, _) = foldl addVertex ([], [], emptyMap) inds
        addVertex (vs, is, imap) id =
          case imap id of
            Nothing -> let next = length vs in
              (vs ++ [mkV id],
               is ++ [next],
               insertIndex imap id next)
            Just x -> (vs, is ++ [x], imap)
        mkV (pi, Nothing, Nothing) = (pos!!(pi-1), Vec 0 0 0, UV 0 0)
        mkV (pi, Just ui, Nothing) = (pos!!(pi-1), Vec 0 0 0, uvs!!(ui-1))
        mkV (pi, Just ui, Just ni) = (pos!!(pi-1), norms!!(ni-1), uvs!!(ui -1))
        mkV (pi, Nothing, Just ni) = (pos!!(pi-1), norms!!(ni-1), UV 0 0)

buildMesh :: [OBJToken] -> Mesh
buildMesh tokens = Mesh (V.fromList verts) ids
  where (verts, ids) = flattenVertices . buildMeshData $ tokens
        
readOBJ :: String -> IO (Mesh)
readOBJ path = do
  content <- readFile path
  return . buildMesh . catMaybes $ map readLine (lines content)
