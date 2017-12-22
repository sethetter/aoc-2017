module Lib
    ( parseGrid
    , initCarrier
    , doBurst
    , getCount
    ) where

import qualified Data.Map.Strict as M
import Data.List (foldl')

data Dir = N | E | S | W deriving (Show)

data Carrier = Carrier (Int, Int) Dir Int deriving (Show)

type Grid = M.Map (Int, Int) Int

parseGrid :: [String] -> Grid
parseGrid strs =
  let coords = [ (x, y) | x <- [0..length strs - 1], y <- [0..length strs - 1] ]
   in foldl' (\m c@(x, y) -> let v = if strs!!y!!x == '#' then 1 else 0
                              in M.insert c v m
             ) M.empty coords

startingPos :: Grid -> (Int, Int)
startingPos grid = case M.lookupMax grid of
  Just ((x, y), _) -> ((div x 2), (div y 2))
  Nothing -> (0, 0)

initCarrier :: Grid -> Carrier
initCarrier g = Carrier (startingPos g) N 0

doBurst :: (Grid, Carrier) -> (Grid, Carrier)
doBurst (grid, c@(Carrier pos _ _)) =
  case M.lookup pos grid of
    Just v -> case v of
      0 -> (M.adjust (\_ -> 1) pos grid, turnAndMoveLeft . increaseCount $ c)
      _ -> (M.adjust (\_ -> 0) pos grid, turnAndMoveRight c)
    Nothing -> (M.insert pos 1 grid, turnAndMoveLeft . increaseCount $ c)

increaseCount :: Carrier -> Carrier
increaseCount (Carrier pos dir count) = Carrier pos dir (count + 1)

getCount :: Carrier -> Int
getCount (Carrier _ _ c) = c

turnAndMoveRight :: Carrier -> Carrier
turnAndMoveRight (Carrier (x, y) N count) = Carrier (x + 1, y) W count
turnAndMoveRight (Carrier (x, y) W count) = Carrier (x, y + 1) S count
turnAndMoveRight (Carrier (x, y) S count) = Carrier (x - 1, y) E count
turnAndMoveRight (Carrier (x, y) E count) = Carrier (x, y - 1) N count

turnAndMoveLeft :: Carrier -> Carrier
turnAndMoveLeft (Carrier (x, y) N count) = Carrier (x - 1, y) E count
turnAndMoveLeft (Carrier (x, y) E count) = Carrier (x, y + 1) S count
turnAndMoveLeft (Carrier (x, y) S count) = Carrier (x + 1, y) W count
turnAndMoveLeft (Carrier (x, y) W count) = Carrier (x, y - 1) N count


   -- if current infected, turn right, otherwise turn left
   -- if infected
