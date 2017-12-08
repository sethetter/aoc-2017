module Main where

import Lib
import Data.List.Split

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = splitOn "\n" $ init inputStr
      registers = processInstructions input
      largestValue = findLargestValue registers
   in print largestValue
