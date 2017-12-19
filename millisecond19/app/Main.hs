module Main where

import Data.List.Split (splitOn)
import Lib

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = splitOn "\n" $ init inputStr
      path = findPath input
   in print $ path
