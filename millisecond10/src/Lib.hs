module Lib
    ( processLengths
    ) where

processLengths :: [Int] -> [Int]
processLengths = go 0 0 [0..255]
  where go :: Int -> Int -> [Int] -> [Int] -> [Int]
        go _ _    rope []     = rope
        go p skip rope (l:ls) =
          go (mod (p + l + skip) 256) (skip + 1) (twistRope p l rope) ls

twistRope :: Int -> Int -> [Int] -> [Int]
twistRope p l rope =
  let frontEnd = (p + l) - 256
      newSection = reverse . take l . drop p $ cycle rope
   in (drop (l - frontEnd) newSection) ++ (drop frontEnd . take p $ rope) ++
      (take (l - frontEnd) newSection) ++ (drop (p + l) rope)
