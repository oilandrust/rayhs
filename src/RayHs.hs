{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Data.Maybe
import Data.List
import Data.Function
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS

import Text.Printf
import System.Environment (getArgs)
import System.Console.GetOpt
import System.CPUTime
import System.Random

import Math
import Color
import ColorMap
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
import JSON

import qualified Vec (o)
import qualified Geometry as Geom (intersection)

{- Raytracing -}
data Rendering = Rendering { scene :: Scene
                           , camera :: Camera
                           , width :: Int
                           , height :: Int
                           , maxDepth :: Int }

buildRendering :: RenderDesc -> IO Rendering
buildRendering (RenderDesc sceneDesc proj w h d) = do
  scene <- buildScene sceneDesc
  return $ Rendering scene proj w h d

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
  (Point p _ _) -> sqrDist origin p > sqrDist origin pos

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

tracePixel :: Scene -> Int ->  Double -> Double -> Camera
              -> (Double, Double) -> Color
tracePixel scene d w h cam (i, j) = traceRay scene 0 d ray
  where ray = rayFromPixel w h cam i j

rayTrace :: Rendering -> Image
rayTrace (Rendering scene camera w h d) = generatePixels w h
                                          (tracePixel scene d
                                           (fromIntegral w)
                                           (fromIntegral h)
                                           camera)

{- Distributed ray tracer -}
average :: [Color] -> Color
average colors = mul (1.0 / (fromIntegral . length $ colors)) sumc
  where sumc = foldl (+) black colors

distributedTracePixel :: Scene -> Int -> Double -> Double -> Camera
                         -> (Double, Double) -> Rnd Color
distributedTracePixel scene d w h proj (i, j) = do
  samples <- sampleUniform01 128
  let pixelSamples = makePixelSamples samples
  let rays = map
             (\(oi, oj) ->
               rayFromPixel w h proj (i+oi) (j+oj))
             pixelSamples
  let colors = map (traceRay scene 0 d) rays
  return $ average colors

makePixelSamples :: [Double] -> [(Double, Double)]
makePixelSamples [] = []
makePixelSamples (x:y:xs) = ((x-0.5, y-0.5):makePixelSamples xs)
makePixelSamples _ = []

distributedRayTrace :: Rendering -> Rnd Image
distributedRayTrace (Rendering scene camera w h d) =
  generatePixelsRnd w h (distributedTracePixel scene d
                         (fromIntegral w)
                         (fromIntegral h)
                         camera)

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  content <- BS.readFile path
  return $ decode content


{- Command line options -}
data Flag = Output String deriving Show

options :: [OptDescr Flag]
options = [Option ['o'] ["output"] (OptArg makeOutput "FILE")
           "Specify the ouput filename." ]

header = "Usage: main [OPTION...]"

makeOutput :: Maybe String -> Flag
makeOutput = Output . fromMaybe "out.ppm"

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, (fn:_), []) -> do
      let outFile = case flags of
            [] -> "out.ppm"
            (Output fn:_) -> fn
      putStrLn $ "Loading scene from " ++ fn ++ "..."
      jobDesc <- parseFile $ fn
      case jobDesc of
        Nothing -> error "Failed to read scene"
        Just jd -> do
          putStrLn $ "Rendering..."
          job <- buildRendering jd
          let simpleRay = rayTrace job
          writePPM outFile simpleRay
          putStrLn $ "Done! Output written to " ++ outFile
    (_, [], []) -> error $ "No input."
    (_, _, msgs) -> error $ concat msgs ++ usageInfo header options




--let multipleRay = runRandom (distributedRayTrace job) 24
  --writePPM "dist.ppm" multipleRay
