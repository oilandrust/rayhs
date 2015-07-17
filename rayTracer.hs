{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-
TOTO:
-Triangle Mesh
-Correct transmitted radiance
-Multisampling
-Texturing
-kDTree

-Transparent shadows
-Glossy BRDFs
-Path Tracing
-}

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
import Color hiding (mul)
import Image
import Vec hiding (o, mul)
import Material
import Geometry hiding (intersection)
import Mesh
import Projection

import qualified Vec (o, mul)
import qualified Color as C (mul)
import qualified Geometry as Geom (intersection)

{- Light -}
data Light = Directional { dir :: Vec, col :: Color }
           | Point { pos :: Vec, col :: Color, r :: Double } deriving Show

lightAt :: Light -> Vec -> (Vec, Color)
lightAt (Directional d c) _ = (d, c)
lightAt (Point p0 c r) p = (Vec.mul (1/d) (p0 - p), C.mul falloff c)
  where falloff = 1.0 / (1.0 + d/r)^(2::Int)
        d = dist p0 p

{- Raytracing -}
data Object =  Object Geometry Material

data Scene = Scene { shapes :: [Object], lights :: [Light] }

addMesh :: Scene -> Mesh -> Material -> Scene
addMesh (Scene s l) m mat = Scene (s++[Object (geom m) mat]) l

data MatHit = MatHit { position :: Vec
                     , normal :: Vec
                     , time :: Double
                     , material :: Material } deriving Show

intersection :: Ray -> Object -> Maybe MatHit
intersection ray (Object shape material) = do
  (Hit p n t) <- Geom.intersection ray shape
  return (MatHit p n t material)

intersections :: [Object] -> Ray -> [MatHit]
intersections scene ray = catMaybes $ map (intersection ray) scene

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
        isOccluder (MatHit _ _ _ (Emmit _)) = False
        isOccluder _ = True
        inFrontOfLight hit = case light of
          (Directional _ _) -> True
          (Point p _ _) -> (sqrDist (origin ray) p) >
                           (sqrDist (position hit) (origin ray))

accumDiffuse :: [Object] -> [Light] -> Vec -> Vec -> Color -> Color
accumDiffuse sc lts p n color =
  foldl (\c l -> c + (diffFromLight l)) black lts
  where diffFromLight light =
          let inter = shadowIntersection sc light (rayEps p ld)
          in case inter of
            Just _ -> black
            Nothing -> (diffuse color lc ld n)
          where (ld, lc) = lightAt light p

specular :: Scene -> Int -> Vec -> Vec -> Vec -> Color
specular scene depth v p n
  | depth < maxDepth = C.mul (dot (reflect v n) n)
                       (traceRay scene
                       (rayEps p (reflect v n)) (depth+1))
  | otherwise = black
        
{- Irradiance comutation at a point with normal and material -}
irradiance :: Int -> Scene -> Material -> Vec -> Vec -> Vec -> Color

{- Diffuse + Ambient -}
irradiance _ (Scene shapes lights) (Diffuse cd) _ p n = -- (C.mul 0.2 cd) +
  accumDiffuse shapes lights p n cd

{- Plastic -}
irradiance d scene@(Scene shapes lights) (Plastic cd ior) v p n =
  accumDiffuse shapes lights p n cd
  + (C.mul (fresnel ior (dot n (-v))) (specular scene d v p n))

{- Mirror -}
irradiance d scene (Mirror ior) v p n =
  (C.mul (fresnel ior (dot n (-v))) (specular scene d v p n))

{- Emmitter -}
irradiance _ _ (Emmit ce) _ _ _ = ce 

{- Transparent -}
irradiance depth scene (Transparent ior) v p n =
  case transmitedRadiance of
    Nothing -> (C.mul (fresnel ior (dot n (-v))) (specular scene depth v p n))
    Just radiance -> (C.mul (1-(r0 ior 1.0)) radiance)
                     + (C.mul (fresnel ior (dot n (-v)))
                        (specular scene depth v p n))
  where transmitedRadiance  
          | depth == maxDepth = Nothing
          | otherwise = do
            refRay <- (liftM $ rayEps p) (refract v n 1.0 ior)
            (MatHit outp outn _ _) <- (closestIntersection (shapes scene)) refRay
            outRay <- liftM (rayEps outp)
                      (refract (direction refRay) (-outn) ior 1.0)
            return $ traceRay scene outRay (depth+1)
           
traceRay :: Scene -> Ray -> Int -> Color
traceRay scene ray@(Ray _ v) depth =
  let inter = closestIntersection (shapes scene) ray
  in case inter of
    Just (MatHit p n _ mat) -> irradiance depth scene mat v p n
    Nothing -> black

tracePixel :: Scene -> Double -> Double -> (Double, Double) -> Color
tracePixel scene w h (i, j) = traceRay scene ray 0
  where ray = (rayFromPixel w h
               (Perspective 0 2 2 0.1)
               i j)

rayTrace :: Scene -> Int -> Int -> Image
rayTrace scene w h = generatePixels w h
                     (tracePixel scene
                      (fromIntegral w)
                      (fromIntegral h))

{- Test Data -}

topLight :: Light
topLight = Point (Vec 0 0.5 1) (gray 300) 0.1

testMesh :: Scene
testMesh = Scene [Object (geom $ Plane (Vec 0 0 2) (-zAxis)) (Diffuse (gray 2)),
                  Object (geom $ Plane (Vec 1 0 0) (-xAxis)) (Diffuse green),
                  Object (geom $ Plane (Vec (-1) 0 0) (xAxis)) (Diffuse red),
                  Object (geom $ Plane (Vec 0 1 0) (-yAxis)) (Diffuse (gray 2)),
                  Object (geom $ Plane (Vec 0 (-1) 0) yAxis) (Plastic (gray 2) 2)]
           [topLight, Point lightPos (gray 50) 0.1]
  where lightPos = Vec 0 (-0.4) 0.2

out :: Scene
out = Scene [Object (geom $ Sphere (Vec 0.5 (-0.5) 3.2) 0.5) (Plastic red 1.9),
             Object (geom $ Sphere (Vec (-0.7) (-0.6) 3.3) 0.4) (Mirror 0.1),
             Object (geom $ Sphere (Vec (-0.1) (-0.8) 2) 0.2) (Diffuse green),
             Object (geom $ Sphere (Vec 0 2 3) 0.1) (Emmit (gray 0.8)),
             Object (geom $ Plane (Vec 0 (-1) 0) yAxis) (Plastic white 1.7)]
            [Directional (normalize (Vec 1 0.7 (-1))) (gray 1.2),
             Point (Vec 0 1.5 3) (gray 2000) 0.1]

cBox :: Scene
cBox = Scene [Object (geom $ Sphere (Vec 0.5 (-0.6)    1) 0.4) (Plastic red 1.9),
              Object (geom $ Sphere (Vec (-0.4) (-0.7) 1.2) 0.3) (Mirror 0.1),
              Object (geom $ Sphere (Vec (-0.2) (-0.8) 0.4) 0.2) (Diffuse green),
              Object (geom $ Sphere (Vec (0.1) (-0.3) 0.3) 0.2) (Transparent 1.5),
              Object (geom $ Sphere lightPos 0.1) (Emmit white),
              Object (geom $ Plane (Vec 0 0 2) (-zAxis)) (Diffuse (gray 2)),
              Object (geom $ Plane (Vec 1 0 0) (-xAxis)) (Diffuse green),
              Object (geom $ Plane (Vec (-1) 0 0) (xAxis)) (Diffuse red),
              Object (geom $ Plane (Vec 0 1 0) (-yAxis)) (Diffuse (gray 2)),
              Object (geom $ Plane (Vec 0 (-1) 0) yAxis) (Plastic (gray 2) 2)]
       [Point (Vec 0 0.9 0.75) (gray 200) 0.1,
        Point lightPos (gray 50) 0.1]
  where lightPos = Vec 0.6 (-0.4) 0.2


maxDepth :: Int
maxDepth = 5

main :: IO ()
main = do
  args <- getArgs
  mesh <- readOBJ "test.obj"
  let tran = translate mesh (Vec (-0.6) (-0.8) 1.5)
  let output = rayTrace
               (addMesh testMesh tran (Diffuse white))
               512 512
  writePPM "out.ppm" output
  putStrLn "done!"
