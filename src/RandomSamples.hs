module RandomSamples (Rnd
                     , runRandom
                     , sampleUniform01
                     , sampleUniformSq) where

import Control.Monad
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
import Control.Applicative ((<$>))

{- Type encapsulating the state of a random generator -}
type Rnd a = State StdGen a

{- Run a computation that takes random samples -}
runRandom :: Rnd a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

{- get a random sample an pass the state along -}
rand :: Rnd Double
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

randPair :: Rnd (Double, Double)
randPair = do
  x <- rand
  y <- rand
  return (x, y)

{- Uniform distributions -}
uniform01 :: Rnd [Double]
uniform01 = mapM (\_ -> rand) $ repeat ()

uniform :: Double -> Double -> Rnd [Double]
uniform a b = mapM (\_ -> (\s -> s * (b - a) + a) <$> rand) $ repeat ()

uniform2 :: Double -> Rnd [(Double, Double)]
uniform2 s = mapM (\_ -> (\(x, y) -> (2 * s * (x-0.5), 2 * s * (y-0.5)))
                         <$> randPair) $ repeat ()

{- Get n samples uniformly distributed between 0 and 1 -}
sampleUniform01 :: Int -> Rnd [Double]
sampleUniform01 n = sequence $ replicate n rand

{- Get n samples uniformly distributed in a square centerd at the origin
of size s -}
sampleUniformSq :: Double -> Int -> Rnd [(Double, Double)]
sampleUniformSq halfSize n = liftM (take n) (uniform2 halfSize)
