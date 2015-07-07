{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-
TOTO:
-Transparent shadows
-Correct transmitted radiance
-Multisampling
-Texturing
-Triangle Mesh
-kDTree
-Glossy BRDFs
-Path Tracing
-Parallelism
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

import qualified Vec (o, mul)
import qualified Color as C (mul)

{- Raytracing -}
eps :: Float
eps = 0.0001

data Geometry = Sphere { center :: Vec, radius :: Float }
              | Plane Vec Vec
              deriving Show

data Shape = Shape Geometry Material deriving Show

{- Light -}
data Light = Directional { dir :: Vec, col :: Color }
           | Point { pos :: Vec, col :: Color, r :: Float } deriving Show

lightAt :: Light -> Vec -> (Vec, Color)
lightAt (Directional d c) _ = (d, c)
lightAt (Point p0 c r) p = (Vec.mul (1/d) (p0 - p), C.mul falloff c)
  where falloff = 1.0 / (1.0 + d/r)^(2::Int)
        d = dist p0 p

data Scene = Scene { shapes :: [Shape], lights :: [Light] } deriving Show

data Ray = Ray { origin :: Vec
               , direction :: Vec} deriving Show

data Hit = Hit { position :: Vec
               , normal :: Vec
               , t :: Float
               , material :: Material } deriving Show

rayAt :: Ray -> Float -> Vec
rayAt (Ray o d) t = o + Vec.mul t d

intersection :: Ray -> Shape -> Maybe Hit
intersection ray@(Ray o d) (Shape s@(Sphere ct r) color)
  | delta < 0.0 = Nothing
  | t0 > 0 = Just (Hit p0 n0 t0 color)
  | t1 > 0 = Just (Hit p1 n1 t1 color)
  | otherwise = Nothing
  where delta = b ^ (2 :: Int) - 4.0*a*c
        a = dot d d
        b = 2.0 * (dot d (o - ct))
        c = (sqrLen (o - ct)) - r ^ (2 :: Int)
        t0 = 0.5 * ((-b) - (sqrt delta)) / a
        p0 = rayAt ray t0
        n0 = normalize (p0 - ct)
        t1 = 0.5 * ((-b) + (sqrt delta)) / a
        p1 = rayAt ray t1
        n1 = normalize (p1 - ct)

intersection ray@(Ray o d) (Shape (Plane p n) color)
  | (abs dDotn) > 0 && t > 0 = Just (Hit (rayAt ray t) n t color)
  | otherwise = Nothing
    where dDotn = dot d n
          t = dot n (p - o) / dDotn
          
intersections :: [Shape] -> Ray -> [Hit]
intersections scene ray = map fromJust
                          (filter isJust (map (intersection ray) scene))

closestIntersection :: [Shape] -> Ray -> Maybe Hit
closestIntersection scene r = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` t) hits
  where hits = intersections scene r

{- trace ray towards light to see if occluded -}
shadowIntersection :: [Shape] -> Light -> Ray -> Maybe Hit
shadowIntersection scene light r@(Ray o d) = case hits of
  [] -> Nothing
  _  -> Just $ minimumBy (compare `on` t) hits
  where hits = filter isOccluder (filter inFrontOfLight (intersections scene r))
        isOccluder (Hit _ _ _ (Emmit _)) = False
        isOccluder _ = True
        inFrontOfLight hit = case light of
          (Directional _ _) -> True
          (Point p _ _) -> (sqrDist o p) > (sqrDist (position hit) o)

data Projection = Orthographic { width :: Float, height :: Float}
                | Perspective { fovy :: Float
                              , width :: Float
                              , height :: Float
                              , near :: Float }
                  deriving Show

{- Create ray from camera givent the projection -}
rayFromPixel :: Float -> Float -> Projection -> Float -> Float -> Ray
rayFromPixel w h (Orthographic pw ph) x y = Ray o (Vec 0 0 1) where
  o = Vec (apw * (x - (w/2)) / w) (aph * ((-y) + (h/2)) / h) 0
  aspect = w / h
  (apw, aph) = case (aspect>1) of
    True -> (pw, pw/aspect)
    False -> (aspect*ph, ph)

rayFromPixel w h (Perspective f pw ph n) x y = Ray o d where
  o = Vec (apw * (x - (w/2)) / w) (aph * ((-y) + (h/2)) / h) 0
  d = normalize $ (o - (Vec 0 0 (-2)))
  aspect = w / h
  (apw, aph) = case (aspect>1) of
    True -> (pw, pw/aspect)
    False -> (aspect*ph, ph)

rayEps :: Vec -> Vec -> Ray
rayEps p n = Ray (p + (Vec.mul eps n)) n

accumDiffuse :: [Shape] -> [Light] -> Vec -> Vec -> Color -> Color
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
  | depth < maxDepth = traceRay scene
                       (rayEps p (reflect v n)) (depth+1)
  | otherwise = black
        
{- Irradiance comutation at a point with normal and material -}
irradiance :: Int -> Scene -> Material -> Vec -> Vec -> Vec -> Color  
irradiance d (Scene shapes lights) (Diffuse cd) _ p n = (C.mul 0.2 cd) +
  accumDiffuse shapes lights p n cd

irradiance d scene@(Scene shapes lights) (Plastic cd ior) v p n =
  accumDiffuse shapes lights p n cd
  + (C.mul (fresnel ior (dot n (-v))) (specular scene d v p n))

irradiance d scene (Mirror ior) v p n =
  (C.mul (fresnel ior (dot n (-v))) (specular scene d v p n))

irradiance _ _ (Emmit ce) _ _ _ = ce 

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
            (Hit outp outn _ _) <- (closestIntersection (shapes scene)) refRay
            outRay <- liftM (rayEps outp)
                      (refract (direction refRay) (-outn) ior 1.0)
            return $ traceRay scene outRay (depth+1)
           
refract :: Vec -> Vec -> Float -> Float -> Maybe Vec
refract i n n1 n2
  | sin2θ > 1 = Nothing
  | otherwise = Just $ (Vec.mul n1n2 i)
                + (Vec.mul coeff n)
  where n1n2 = n1 / n2
        cosθ = (-(dot i n))
        sin2θ = n1n2 * n1n2 * (1 - cosθ*cosθ)
        coeff = n1n2 * cosθ - (sqrt (1.0-sin2θ))

traceRay :: Scene -> Ray -> Int -> Color
traceRay scene ray@(Ray _ v) depth =
  let inter = closestIntersection (shapes scene) ray
  in case inter of
    Just (Hit p n _ mat) -> irradiance depth scene mat v p n
    Nothing -> black

tracePixel :: Scene -> Float -> Float -> (Float, Float) -> Color
tracePixel scene w h (i, j) = traceRay scene ray 0
  where ray = (rayFromPixel w h
               (Perspective 0 2 2 0.1)
               i j)

rayTrace :: Scene -> Int -> Int -> Image
rayTrace scene w h = generatePixels w h
                     (tracePixel scene
                      (fromIntegral w)
                      (fromIntegral h))

openScene :: Scene
openScene = Scene [Shape (Sphere (Vec 0.5 (-0.5)    3.2) 0.5) (Plastic red 1.9),
                   Shape (Sphere (Vec (-0.7) (-0.6) 3.3) 0.4) (Mirror 0.1),
                   Shape (Sphere (Vec (-0.1) (-0.8) 2.1) 0.2) (Diffuse green),
                   Shape (Sphere (Vec 0 2 3) 0.1) (Emmit (gray 0.8)),
                   Shape (Plane (Vec 0 (-1) 0) yAxis) (Plastic white 1.7)]
                  [Directional (normalize (Vec 1 0.7 (-1))) (gray 1.2),
                   Point (Vec 0 1.5 3) (gray 1000) 0.1]


cornellBox :: Scene
cornellBox = Scene [Shape (Sphere (Vec 0.5 (-0.6)    1) 0.4) (Plastic red 1.9),
                    Shape (Sphere (Vec (-0.4) (-0.7) 1.2) 0.3) (Mirror 0.1),
                    Shape (Sphere (Vec (-0.2) (-0.8) 0.4) 0.2) (Diffuse green),
                    Shape (Sphere (Vec (0.1) (-0.3) 0.3) 0.2) (Transparent 1.5),
                    Shape (Sphere lightPos 0.1) (Emmit white),
                    Shape (Plane (Vec 0 0 2) (-zAxis)) (Diffuse (gray 2)),
                    Shape (Plane (Vec 1 0 0) (-xAxis)) (Diffuse green),
                    Shape (Plane (Vec (-1) 0 0) (xAxis)) (Diffuse red),
                    Shape (Plane (Vec 0 1 0) (-yAxis)) (Diffuse (gray 2)),
                    Shape (Plane (Vec 0 (-1) 0) yAxis) (Plastic (gray 2) 2)]
                   [Point (Vec 0 0.9 0.75) (gray 200) 0.1,
                    Point lightPos (gray 50) 0.1]
  where lightPos = Vec 0.6 (-0.4) 0.2

testTransparent :: Scene
testTransparent = Scene
                  [Shape (Sphere (Vec 0 0 1) 0.5) (Transparent 1.5),
                   Shape (Plane (Vec 0 0 2) (-zAxis)) (Diffuse (gray 2)),
                   Shape (Plane (Vec 1 0 0) (-xAxis)) (Diffuse green),
                   Shape (Plane (Vec (-1) 0 0) (xAxis)) (Diffuse red),
                   Shape (Plane (Vec 0 1 0) (-yAxis)) (Diffuse (gray 2)),
                   Shape (Plane (Vec 0 (-1) 0) yAxis) (Plastic (gray 2) 2)]
                  [Point (Vec 0 0.9 0.75) (gray 200) 0.1]


maxDepth :: Int
maxDepth = 5

main :: IO ()
main = do
  args <- getArgs
  let output = rayTrace cornellBox 1024 1024
  writePPM "out.ppm" output
  putStrLn "done!"
