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
data Mesh = Mesh { vertices :: Vector Vec
                 , normals :: Vector Vec
                 , indices :: [Int] } deriving Show

empty :: Mesh
empty = Mesh V.empty V.empty []

instance Inter Mesh where
  intersection = rayInterMesh

{- Ray Intersection -}
rayInterMesh :: Ray -> Mesh -> Maybe Hit
rayInterMesh ray mesh@(Mesh pts norms ids) = do
  let hits = catMaybes $ mapTriangles (triangleIntersection ray) pts norms ids
    in case hits of
    [] -> Nothing
    xs -> Just $ minimumBy (compare `on` t) xs


-- Convenience type to store 3 vertices or 3 normals, ...
type TriVec = (Vec, Vec, Vec)

triLookup :: Vector Vec -> (Int, Int, Int) -> TriVec
triLookup verts (i, j, k) = (verts ! i, verts ! j, verts ! k)

barycentricInterp :: Double -> Vec ->
                     Double -> Vec ->
                     Double -> Vec -> Vec
barycentricInterp a p b q c r =
  (Vec.mul a p) + (Vec.mul b q) + (Vec.mul c r)

triangleIntersection :: Ray -> TriVec -> TriVec -> Maybe Hit
triangleIntersection ray@(Ray o d) (p0, p1, p2) (n0, n1, n2) = do
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
        n = barycentricInterp u n1 v n2 (1-u-v) n0

translate :: Mesh -> Vec -> Mesh
translate (Mesh v n i) t = Mesh (V.map (+t) v) n i

mapTriangles :: (TriVec -> TriVec -> b) -> Vector Vec -> Vector Vec-> [Int] -> [b]
mapTriangles f pts norms indices =
  map (\(pos, norms) -> f pos norms)
  (map (\ids -> (triLookup pts ids, triLookup norms ids)) (faces indices))
  where faces [] = []
        faces (i:j:k:is) = (i, j, k):(faces is)


{- Obj Loading -}

{-  Obj vertices can be Position, Normal or Texture Coord (not supported yet)
Faces must have a position index and have optionaly a normal index and
tc index
 -}

type OBJIndex = (Int, Maybe Int, Maybe Int)
data OBJToken = Vert Vec
              | Norm Vec
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
readLine ('f':' ':rest) = readFace rest
readLine ('v':'n':' ':rest) = readNormal rest
readLine _ = Nothing

buildMeshData :: [OBJToken] -> ([Vec], [Vec], [OBJIndex])
buildMeshData tokens = foldl dispatch ([], [], []) tokens
 where dispatch (vs, ns, is) (Vert v) = (vs ++ [v], ns, is)
       dispatch (vs, ns, is) (Norm n) = (vs, ns ++ [n], is) 
       dispatch (vs, ns, is) (Face f) = (vs, ns, is ++ f)

-- Functional style lookup table
type IndexMap = OBJIndex -> Maybe Int
emptyMap :: IndexMap
emptyMap _ = Nothing

insertIndex :: IndexMap -> OBJIndex -> Int -> IndexMap
insertIndex imap oi i query = case query == oi of
  True -> Just i
  False -> imap query

flatten :: ([Vec], [Vec], [OBJIndex]) -> ([Vec], [Vec], [Int])
flatten (pos, norms, inds) = (pts, nrs, ids)
  where (pts, nrs, ids, _) = foldl addVertex ([], [], [], emptyMap) inds
        addVertex (vs, ns, is, imap) id@(pi, _, Just ni) =
          case imap id of
            Nothing -> let next = length vs in
              (vs ++ [pos !! (pi-1)],
               ns ++ [norms !! (ni-1)],
               is ++ [next],
               insertIndex imap id next)
            Just x -> (vs, ns, is ++ [x], imap)
           
buildMesh :: [OBJToken] -> Mesh
buildMesh tokens = Mesh (V.fromList vs) (V.fromList ns) ids
  where (vs, ns, ids) = flatten . buildMeshData $ tokens
        
readOBJ :: String -> IO (Mesh)
readOBJ path = do
  content <- readFile path
  return . buildMesh . catMaybes $ map readLine (lines content)

