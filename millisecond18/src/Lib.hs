module Lib
    ( firstReceivedSound
    ) where

import qualified Data.Map.Strict as M

type Registers = M.Map Char Int
type Sounds = [Int]

type RSPair = (Sounds, Registers)

firstReceivedSound :: [String] -> Int
firstReceivedSound instructions = go 0 ([], M.empty)
  where go :: Int -> RSPair -> Int
        go i rsPair@(sounds, registers)
          | i < 0 || i >= length instructions = -1
          | otherwise = case head $ words (instructions !! i) of
              "rcv" -> let v = M.findWithDefault 0 (getR $ instructions !! i) registers
                        in if v == 0 then go (i + 1) rsPair
                                     else head sounds
              "jgz" -> let v = M.findWithDefault 0 (getR $ instructions !! i) registers
                        in if v == 0 then go (i + 1) rsPair
                                     else let offset = getVorR (instructions !! i) registers
                                           in go (i + offset) rsPair
              _     -> let operation = parseOp (instructions !! i)
                        in go (i + 1) $ operation rsPair

getR :: String -> Char
getR i = head $ (words i) !! 1

getVorR :: String -> Registers -> Int
getVorR i registers =
  let sndWrd = (words i) !! 2
   in if (head sndWrd) `elem` ['a'..'z'] then M.findWithDefault 0 (head sndWrd) registers
                                         else strToInt sndWrd

parseOp :: String -> RSPair -> RSPair
parseOp str pair@(_, registers) = case head $ words str of
  "snd" -> doSnd (getR str) pair
  "set" -> doSet (getR str) (getVorR str registers) pair
  "add" -> doOp (+) (getR str) (getVorR str registers) pair
  "mul" -> doOp (*) (getR str) (getVorR str registers) pair
  "mod" -> doOp (\x y -> if y == 0 then 0 else mod x y) (getR str) (getVorR str registers) pair
  _     -> pair

doSnd :: Char -> RSPair -> RSPair
doSnd r (sounds, registers) = ((M.findWithDefault 0 r registers) : sounds, registers)

doSet :: Char -> Int -> RSPair -> RSPair
doSet r v (sounds, registers) =
  if M.member r registers then (sounds, M.adjust (\_ -> v) r registers)
                          else (sounds, M.insert r v registers)

doOp :: (Int -> Int -> Int) -> Char -> Int -> RSPair -> RSPair
doOp op r v (sounds, registers) =
  case M.lookup r registers of
    Just _  -> (sounds, M.adjust (\x -> op x v) r registers)
    Nothing -> (sounds, M.insert r (op 0 v) registers)

strToInt :: String -> Int
strToInt s = (read s :: Int)
