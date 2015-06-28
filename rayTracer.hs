{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS



main :: IO ()
main = do
  args <- getArgs
  putStrLn "done!"
