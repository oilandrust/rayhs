module Main (main
            , computeArray) where

import Control.Monad
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Control.Applicative ((<$>))

type Rnd a = State StdGen a

runRandom :: Rnd a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: Rnd Double
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

{- Uniform distributions -}
uniform01 :: Rnd [Double]
uniform01 = mapM (\_ -> rand) $ repeat ()

{- Get n samples uniformly distributed between 0 and 1 -}
sampleUniform :: Int -> Rnd [Double]
sampleUniform n = sequence $ replicate n rand

computeArray :: Int -> Rnd [Bool]
computeArray n = do
  let samples1 = runRandom (sampleUniform 10) 24
  let samples2 = runRandom (sampleUniform 10) 56
  let dat = zip samples1 samples2
  return $ uncurry (<) <$> dat

main :: IO ()
main = do
  let seed = 48
  let res = runRandom computeArray seed
  putStrLn $ show res
