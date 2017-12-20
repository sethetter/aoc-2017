module Main where

import Lib
import Data.List.Split (splitOn)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = splitOn "\n" $ init inputStr
      particles = parseParticles input
      closest = closestToZeroOverXSteps 2000 particles
   in print $ closest
