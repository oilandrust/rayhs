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
import System.Random

import Math
import Color
import Image
import Vec hiding (o)
import Material
import Light
import Scene
import Geometry hiding (intersection)
import Mesh
import Projection
import KDTree
import RandomSamples
import Descriptors

import qualified Vec (o)
import qualified Geometry as Geom (intersection)

{- Raytracing -}
data Rendering = Rendering { scene :: Scene
                           , projection :: Projection
                           , width :: Int
                           , height :: Int
                           , maxDepth :: Int }

data MatHit = MatHit { position :: !Vec
                     , normal :: !Vec
                     , uv :: !UV
                     , time :: !Double
                     , material :: !Material } deriving Show

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
  where hits = filter isOccluder (filter
                                  (inFrontOfLight light ray)
                                  (intersections scene ray))
        isOccluder (MatHit _ _ _ _ (Emmit _)) = False
        isOccluder _ = True

inFrontOfLight :: Light -> Ray -> MatHit -> Bool
inFrontOfLight light (Ray origin _) (MatHit pos _ _ _ _) = case light of
  (Directional _ _) -> True
  (Point p _ _) -> sqrDist origin  p >
                   sqrDist pos origin

accumDiffuse :: [Object] -> [Light] -> Vec -> Vec -> Color -> Color
accumDiffuse sc lts p n color =
  foldl (\c l -> c + diffFromLight l) black lts
  where diffFromLight light =
          let inter = shadowIntersection sc light (rayEps p ld)
          in case inter of
            Just _ -> black
            Nothing -> diffuse color lc ld n
          where (ld, lc) = lightAt light p

specular :: Scene -> Int -> Int -> Vec -> Vec -> Vec -> Color
specular scene depth maxDepth v p n
  | depth < maxDepth = mul (dot (reflect v n) n)
                       (traceRay scene (depth+1) maxDepth
                       (rayEps p (reflect v n)))
  | otherwise = black

{- Irradiance comutation at a point with view direction, normal and material -}
irradiance :: Int -> Int -> Scene -> Material ->
              Direction -> Position -> Normal -> UV -> Color

{- Diffuse + Ambient -}
irradiance _ _ (Scene shapes lights) (Diffuse cdMap) _ p n uv =
  mul 0.2 cd +
  accumDiffuse shapes lights p n cd
  where cd = colorAt cdMap uv

{- Plastic -}
irradiance d maxDepth scene@(Scene shapes lights) (Plastic cdMap ior) v p n uv =
  accumDiffuse shapes lights p n cd
  + mul (fresnel ior (dot n (-v))) (specular scene d maxDepth v p n)
  where cd = colorAt cdMap uv

{- Mirror -}
irradiance d maxDepth scene (Mirror ior) v p n _ =
  mul (fresnel ior (dot n (-v))) (specular scene d maxDepth v p n)

{- Emmitter -}
irradiance _ _ _ (Emmit ce) _ _ _ _ = ce

{- Transparent -}
irradiance d maxDepth scene (Transparent ior) v p n _ =
  case transmitedRadiance of
    Nothing -> mul (fresnel ior (dot n (-v))) (specular scene d maxDepth v p n)
    Just radiance -> mul (1 - r0 ior 1.0) radiance
                     + mul (fresnel ior (dot n (-v)))
                     (specular scene d maxDepth v p n)
  where transmitedRadiance
          | d == maxDepth = Nothing
          | otherwise = do
            refr@(Ray _ refDir) <- (liftM $ rayEps p) (refract v n 1.0 ior)
            (MatHit outp outn _ _ _) <- closestIntersection (shapes scene) refr
            outRay <- liftM (rayEps outp)
                      (refract refDir (-outn) ior 1.0)
            return $ traceRay scene (d+1) maxDepth outRay

{- Debug material -}
irradiance _ _ _ ShowNormal _ _ n _ = toRGB n
irradiance _ _ _ ShowUV _ _ _ (UV u v) = RGB u v 0

traceRay :: Scene -> Int -> Int -> Ray -> Color
traceRay scene depth maxDepth ray@(Ray _ v) =
  let inter = closestIntersection (shapes scene) ray
  in case inter of
    Just (MatHit p n uv _ mat) -> irradiance depth maxDepth scene mat v p n uv
    Nothing -> black

tracePixel :: Scene -> Int ->  Double -> Double -> Projection
              -> (Double, Double) -> Color
tracePixel scene d w h proj (i, j) = traceRay scene 0 d ray
  where ray = rayFromPixel w h proj i j 0 0

rayTrace :: Rendering -> Image
rayTrace (Rendering scene projection w h d) = generatePixels w h
                                              (tracePixel scene d
                                               (fromIntegral w)
                                               (fromIntegral h)
                                               projection)

{- Distributed ray tracer -}
average :: [Color] -> Color
average colors = mul (1.0 / (fromIntegral . length $ colors)) sumc
  where sumc = foldl (+) black colors

distributedTracePixel :: Scene -> Int -> Double -> Double -> Projection
                         -> (Double, Double) -> Rnd Color
distributedTracePixel scene d w h proj (i, j) = do
  samples <- sampleUniform01 512
  let pixelSamples = makePixelSamples samples
  let rays = map
             (\(oi, oj, ci, cj) ->
               rayFromPixel w h proj (i+oi) (j+oj) (0.05*ci) (0.05*cj))
             pixelSamples
  let colors = map (traceRay scene 0 d) rays
  return $ average colors

makePixelSamples :: [Double] -> [(Double, Double, Double, Double)]
makePixelSamples [] = []
makePixelSamples (x:y:z:w:xs) = ((x-0.5, y-0.5, z, w):makePixelSamples xs)
makePixelSamples _ = []

distributedRayTrace :: Rendering -> Rnd Image
distributedRayTrace (Rendering scene projection w h d) =
  generatePixelsRnd w h (distributedTracePixel scene d
                         (fromIntegral w)
                         (fromIntegral h)
                         projection)

{- Test Data -}
outScene :: SceneDesc
outScene = SceneDesc
           [ObjectDesc (SphereDesc (Vec 0.5 (-0.5) 3.2) 0.5)
            (Plastic (CheckerBoard red yellow 0.2) 1.9),
            ObjectDesc (SphereDesc (Vec (-0.7) (-0.6) 3.3) 0.4) (Mirror 0.1),
            ObjectDesc (SphereDesc (Vec (-0.1) (-0.8) 2) 0.2)
            (Diffuse $ Flat green),
            ObjectDesc (SphereDesc (Vec 0 2 3) 0.1) (Emmit $ gray 0.8),
            ObjectDesc (PlaneDesc (Vec 0 (-1) 0) yAxis xAxis)
            (Plastic (CheckerBoard black white 2) 1.7),
            ObjectDesc (MeshDesc "data/cube.obj" boxPos)
            (Plastic (Flat $ gray 1.5) 1.9),
            ObjectDesc (MeshDesc "data/torus.obj" (boxPos+torusOffset))
            (Plastic (Flat yellow) 1.7),
            ObjectDesc (MeshDesc "data/uvtorus.obj" (Vec (-0.4) (-1) 0.5))
             (Diffuse (CheckerBoard blue white 2))]
           [Directional (normalize (Vec 1 0.7 (-1))) (gray 1),
            Point (Vec 0 1.5 3) (gray 1000) 0.1]
  where boxPos = Vec (-2) (-0.6) 4
        torusOffset = Vec 0 0.75 0

{-
cBox :: SceneDesc
cBox = SceneDesc
       [(SphereDesc (Vec 0.5 (-0.6) 1) 0.4,
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
         Plastic (Flat yellow) 1.7)]
       -- lights
       [Point (Vec 0 0.9 0.75) (gray 200) 0.1,
        Point lightPos (gray 50) 0.1]
  where lightPos = Vec 0.6 (-0.4) 0.2
        boxPos = Vec (-0.4) (-0.6) 1.5
        torusOffset = Vec 0 0.75 0

fresn :: SceneDesc
fresn = SceneDesc
        [(MeshDesc "data/dragon.obj" (Vec 0 (-1) (0.5)),
          Plastic (Flat red) 1.9),
         (PlaneDesc (Vec 0 0 2) (-zAxis) xAxis,
          Diffuse $ Flat (gray 2)),
         (PlaneDesc (Vec 1 0 0) (-xAxis) yAxis,
          Diffuse $ Flat green),
         (PlaneDesc (Vec (-1) 0 0) xAxis yAxis,
          Diffuse $ Flat red),
         (PlaneDesc (Vec 0 1 0) (-yAxis) zAxis,
          Diffuse $ Flat (gray 1.5)),
         (SphereDesc lightPos 0.1, Emmit white),
         (SphereDesc lightPos2 0.07, Emmit white),
         (PlaneDesc (Vec 0 (-1) 0) yAxis zAxis,
          Plastic (CheckerBoard black (gray 2) 0.5) 2)]
         -- lights
        [Point (Vec 0 0.9 0.75) (gray 150) 0.1,
         Point lightPos2 (gray 50) 0.1,
         Point lightPos (gray 50) 0.1]
  where lightPos2 = Vec (-0.5) (-0.2) 0.12
        lightPos = Vec 0.4 (-0.1) (-0.1)
-}
main :: IO ()
main = do
  scene <- buildScene outScene
  let job = Rendering scene (Perspective 0 2 2 0.1) 512 512 3
  let simpleRay = rayTrace job
  writePPM "out.ppm" simpleRay
--  let multipleRay = runRandom (distributedRayTrace job) 24
--  writePPM "dist.ppm" multipleRay
  putStrLn "done!"
