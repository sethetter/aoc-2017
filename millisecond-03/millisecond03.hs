module Millisecond03 where

data Direction = R | U | L | D

spMemPositionFor :: Int -> (Int, Int)
spMemPositionFor i = go 1 1 1 R True (0, 0)
  where go :: Int -> Int -> Int -> Direction -> Bool -> (Int, Int) -> (Int, Int)
        go c steps 0 d firstTurn coord = if c == i then coord else
          if firstTurn then go (c + 1) steps       steps newDir False nextCoord
                       else go (c + 1) (steps + 1) steps newDir True  nextCoord
            where nextCoord = newCoord (nextDir d) coord
                  newDir    = nextDir d
        go c steps step d firstTurn coord =
          if c == i then coord
                    else go (c + 1) steps (step - 1) d firstTurn (newCoord d coord)

nextDir :: Direction -> Direction
nextDir R = U
nextDir U = L
nextDir L = D
nextDir D = R

newCoord :: Direction -> (Int, Int) -> (Int, Int)
newCoord d (x, y) = case d of
  R -> (x + 1, y)
  U -> (x, y - 1)
  L -> (x - 1, y)
  D -> (x, y + 1)

distanceFromZero :: (Int, Int) -> Int
distanceFromZero (x, y) = x + y

main :: IO ()
main = do
  let coord = spMemPositionFor 361527
      answer = distanceFromZero coord
   in print answer
