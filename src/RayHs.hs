{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Data.Maybe
import Data.List
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

import Text.Printf
import System.Environment (getArgs)
import System.CPUTime

import Math
import Color
import Image
import Vec hiding (o)
import Material
import Geometry hiding (intersection)
import Mesh
import Projection
import KDTree

import qualified Vec (o)
import qualified Geometry as Geom (intersection)

{- Light -}
data Light = Directional { dir :: Vec, col :: Color }
           | Point { pos :: Vec, col :: Color, r :: Double } deriving Show

lightAt :: Light -> Vec -> (Vec, Color)
lightAt (Directional d c) _ = (d, c)
lightAt (Point p0 c r) p = (Vec.mul (1/d) (p0 - p), mul falloff c)
  where falloff = 1.0 / (1.0 + d/r)^(2::Int)
        d = dist p0 p

{- Raytracing -}
data Object = Object Geometry Material

data Scene = Scene { shapes :: [Object], lights :: [Light] }

data MatHit = MatHit { position :: Vec
                     , normal :: Vec
                     , uv :: UV
                     , time :: Double
                     , material :: Material } deriving Show

intersection :: Ray -> Object -> Maybe MatHit
intersection ray (Object shape material) = do
  hit <- Geom.intersection ray shape
  case hit of
    (Hit p n uv t) -> return (MatHit p n uv t material)

intersections :: [Object] -> Ray -> [MatHit]
intersections scene ray = mapMaybe (intersection ray) scene

closestIntersection :: [Object] -> Ray -> Maybe MatHit
closestIntersection scene r = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` time) hits
  where hits = intersections scene r

{- trace ray towards light to see if occluded -}
shadowIntersection :: [Object] -> Light -> Ray -> Maybe MatHit
shadowIntersection scene light ray = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` time) hits
  where hits = filter isOccluder (filter inFrontOfLight (intersections scene ray))
        isOccluder (MatHit _ _ _ _ (Emmit _)) = False
        isOccluder _ = True
        inFrontOfLight hit = case light of
          (Directional _ _) -> True
          (Point p _ _) -> sqrDist (origin ray) p >
                           sqrDist (position hit) (origin ray)

accumDiffuse :: [Object] -> [Light] -> Vec -> Vec -> Color -> Color
accumDiffuse sc lts p n color =
  foldl (\c l -> c + diffFromLight l) black lts
  where diffFromLight light =
          let inter = shadowIntersection sc light (rayEps p ld)
          in case inter of
            Just _ -> black
            Nothing -> diffuse color lc ld n
          where (ld, lc) = lightAt light p

specular :: Scene -> Int -> Vec -> Vec -> Vec -> Color
specular scene depth v p n
  | depth < maxDepth = mul (dot (reflect v n) n)
                       (traceRay scene
                       (rayEps p (reflect v n)) (depth+1))
  | otherwise = black

{- Irradiance comutation at a point with view direction, normal and material -}
irradiance :: Int -> Scene -> Material ->
              Direction -> Position -> Normal -> UV -> Color

{- Diffuse + Ambient -}
irradiance _ (Scene shapes lights) (Diffuse cdMap) _ p n uv =
  mul 0.2 cd +
  accumDiffuse shapes lights p n cd
  where cd = colorAt cdMap uv

{- Plastic -}
irradiance d scene@(Scene shapes lights) (Plastic cdMap ior) v p n uv =
  accumDiffuse shapes lights p n cd
  + mul (fresnel ior (dot n (-v))) (specular scene d v p n)
  where cd = colorAt cdMap uv

{- Mirror -}
irradiance d scene (Mirror ior) v p n _ =
  mul (fresnel ior (dot n (-v))) (specular scene d v p n)

{- Emmitter -}
irradiance _ _ (Emmit ce) _ _ _ _ = ce

{- Transparent -}
irradiance depth scene (Transparent ior) v p n _ =
  case transmitedRadiance of
    Nothing -> mul (fresnel ior (dot n (-v))) (specular scene depth v p n)
    Just radiance -> mul (1 - r0 ior 1.0) radiance
                     + mul (fresnel ior (dot n (-v)))
                     (specular scene depth v p n)
  where transmitedRadiance
          | depth == maxDepth = Nothing
          | otherwise = do
            refr <- (liftM $ rayEps p) (refract v n 1.0 ior)
            (MatHit outp outn _ _ _) <- closestIntersection (shapes scene) refr
            outRay <- liftM (rayEps outp)
                      (refract (direction refr) (-outn) ior 1.0)
            return $ traceRay scene outRay (depth+1)

{- Debug material -}
irradiance _ _ ShowNormal _ _ n _ = toRGB n
irradiance _ _ ShowUV _ _ _ (UV u v) = RGB u v 0

traceRay :: Scene -> Ray -> Int -> Color
traceRay scene ray@(Ray _ v) depth =
  let inter = closestIntersection (shapes scene) ray
  in case inter of
    Just (MatHit p n uv _ mat) -> irradiance depth scene mat v p n uv
    Nothing -> black

tracePixel :: Scene -> Double -> Double -> (Double, Double) -> Color
tracePixel scene w h (i, j) = traceRay scene ray 0
  where ray = rayFromPixel w h (Perspective 0 2 2 0.1) i j

rayTrace :: Scene -> Int -> Int -> Image
rayTrace scene w h = generatePixels w h
                     (tracePixel scene
                      (fromIntegral w)
                      (fromIntegral h))

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

{- Test Data -}
outScene :: SceneDesc
outScene = ([(SphereDesc (Vec 0.5 (-0.5) 3.2) 0.5,
              Plastic (CheckerBoard red yellow 0.2) 1.9),
             (SphereDesc (Vec (-0.7) (-0.6) 3.3) 0.4, Mirror 0.1),
             (SphereDesc (Vec (-0.1) (-0.8) 2) 0.2, Diffuse (Flat green)),
             (SphereDesc (Vec 0 2 3) 0.1, Emmit (gray 0.8)),
             (PlaneDesc (Vec 0 (-1) 0) yAxis xAxis,
              Plastic (CheckerBoard black white 2) 1.7),
             (MeshDesc "data/cube.obj" boxPos,
              Plastic (Flat (gray 1.5)) 1.9),
             (MeshDesc "data/torus.obj" (boxPos+torusOffset),
              Plastic (Flat yellow) 1.7),
             (MeshDesc "data/uvtorus.obj" (Vec (-0.4) (-1) 0.5),
              Diffuse (CheckerBoard blue white 2))],
            -- Lights
            [Directional (normalize (Vec 1 0.7 (-1))) (gray 1),
             Point (Vec 0 1.5 3) (gray 1000) 0.1])
  where boxPos = Vec (-2) (-0.6) 4
        torusOffset = Vec 0 0.75 0

cBox :: SceneDesc
cBox = ([(SphereDesc (Vec 0.5 (-0.6) 1) 0.4,
          Plastic (Flat red) 1.9),
         (SphereDesc (Vec 0.7 0.7 1.7) 0.22, Mirror 0.1),
         (SphereDesc (Vec (-0.4) (-0.8) 0.4) 0.2, Diffuse (Flat green)),
         (SphereDesc (Vec 0.1 (-0.3) 0.3) 0.2, Transparent 1.5),
         (SphereDesc lightPos 0.1, Emmit white),
         (PlaneDesc (Vec 0 0 2) (-zAxis) xAxis,
          Diffuse $ Flat (gray 2)),
         (PlaneDesc (Vec 1 0 0) (-xAxis) yAxis,
          Diffuse $ Flat green),
         (PlaneDesc (Vec (-1) 0 0) xAxis yAxis,
          Diffuse $ Flat red),
         (PlaneDesc (Vec 0 1 0) (-yAxis) zAxis,
          Diffuse $ Flat (gray 2)),
         (PlaneDesc (Vec 0 (-1) 0) yAxis zAxis,
          Plastic (CheckerBoard black (gray 2) 0.25) 2),
         (MeshDesc "data/cube.obj" boxPos,
          Plastic (Flat (gray 1.5)) 1.9),
         (MeshDesc "data/torus.obj" (boxPos+torusOffset),
          Plastic (Flat yellow) 1.7)],
        -- lights
       [Point (Vec 0 0.9 0.75) (gray 200) 0.1,
        Point lightPos (gray 50) 0.1])
  where lightPos = Vec 0.6 (-0.4) 0.2
        boxPos = Vec (-0.4) (-0.6) 1.5
        torusOffset = Vec 0 0.75 0

maxDepth :: Int
maxDepth = 5

main :: IO ()
main = do
  scene <- buildScene cBox
  let output = rayTrace scene 1024 1024
  writePPM "out.ppm" output
  putStrLn "done!"
