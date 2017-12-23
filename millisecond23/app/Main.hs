module Main where

import Lib

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = lines $ init inputStr
      answer1 = howManyMuls input
   in print answer1

