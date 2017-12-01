module Millisecond01 where

import Data.Char

processDigitStr :: String -> Int
processDigitStr str =
  let pairs = zip str (tail str ++ [head str])
   in sum . map (digitToInt . fst) . filter (uncurry (==)) $ pairs

main :: IO ()
main = do
  inputStr <- readFile "input.txt"
  let input = init inputStr -- Drops the \n at the end
      answer = processDigitStr input
   in print answer
