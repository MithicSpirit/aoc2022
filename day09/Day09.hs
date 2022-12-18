#!/usr/bin/env runghc

module Main where

import Data.Bifunctor
import Prelude hiding (Right, Left)
import Control.Monad
import Data.List

main :: IO ()
main = do
  contents <- readFile "day09/input"
  print $ trailNumPos contents
  print $ trailNumPos9Tails contents


trailNumPos :: String -> Int
trailNumPos = parse .> trackTailSteps .> nub .> length

trailNumPos9Tails :: String -> Int
trailNumPos9Tails = parse .> track9TailSteps .> nub .> length


parse :: String -> [Step]
parse = lines .> map (words .> first2 .> bimap stepFromString read)
          >=> uncurry (flip replicate)

data Step = Up | Right | Down | Left

stepFromString :: String -> Step
stepFromString ('U':_) = Up
stepFromString ('R':_) = Right
stepFromString ('D':_) = Down
stepFromString ('L':_) = Left
stepFromString x = error $ "Couldn't convert to step: " ++ show x

trackTailSteps :: [Step] -> [Position]
trackTailSteps = aux (0,0) (0,0) []
  where aux :: Position -> Position -> [Position] -> [Step] -> [Position]
        aux _ t acc [] = t:acc
        aux h t acc (s:ss) = let h' = move h s
                                 t' = trail t h'
                                 in aux h' t' (t:acc) ss

track9TailSteps :: [Step] -> [Position]
track9TailSteps = aux (0,0) (replicate 9 (0,0)) []
  where aux :: Position -> [Position] -> [Position] -> [Step] -> [Position]
        aux _ ts acc [] = last ts : acc
        aux h ts acc (s:ss) = let h' = move h s
                                  ts' = dragTails ts h'
                                  in aux h' ts' (last ts : acc) ss

        dragTails :: [Position] -> Position -> [Position]
        dragTails [] _ = []
        dragTails (t:ts) h = trail t h : dragTails ts t

type Position = (Int,Int)

move :: Position -> Step -> Position
move (x,y) s = case s of
  Up    -> (x,y+1)
  Right -> (x+1,y)
  Down  -> (x,y-1)
  Left  -> (x-1,y)

trail :: Position -> Position -> Position
trail (x1,y1) (x2,y2) = (x1 + dx, y1 + dy)
  where deltaX = x2 - x1
        deltaY = y2 - y1
        toMove = abs deltaX <= 1 && abs deltaY <= 1
        dx = if toMove then 0 else sign deltaX
        dy = if toMove then 0 else sign deltaY


first2 :: [a] -> (a,a)
first2 (a:b:_) = (a,b)
first2 xs = error $ "Too short; length: " ++ show (length xs)

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixr 9 .>

sign :: Int -> Int
sign x | x < 0     = -1
       | x > 0     = 1
       | otherwise = 0
