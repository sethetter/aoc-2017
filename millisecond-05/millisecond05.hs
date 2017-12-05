module Millisecond05 where

import Data.List.Split

jumpsOutInXSteps :: [Int] -> Int
jumpsOutInXSteps = go 0 0
  where go :: Int -> Int -> [Int] -> Int
        go steps pos ints
          | pos < 0  || pos >= (length ints) = steps
          | otherwise = let jump = ints !! pos
                            newList = if jump > 2 then updateIntAt pos (jump - 1) ints
                                                  else updateIntAt pos (jump + 1) ints
                            newPos = (+) pos jump
                         in go (steps + 1) newPos newList

updateIntAt :: Int -> Int -> [Int] -> [Int]
updateIntAt idx int xs = (take idx xs) ++ [int] ++ (drop (idx + 1) xs)

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = map (\l -> read l :: Int) . splitOn "\n" $ init inputStr
      answer = jumpsOutInXSteps input
   in print answer
