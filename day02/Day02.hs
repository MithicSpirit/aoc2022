#!/usr/bin/env runghc

module Main where

import Data.Bifunctor

main :: IO ()
main = do
  contents <- readFile "day02/input"
  print $ countScore contents
  print $ countScore' contents

countScore :: String -> Integer
countScore = sum . map (uncurry scoreTotal) . parseGuide

countScore' :: String -> Integer
countScore' = sum . map (uncurry scoreTotal
                         . \(x,y) -> (x, toShape x y)) . parseGuide'

data RPS = Rock | Paper | Scissors

scoreShape :: RPS -> Integer
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreMatch :: RPS -> RPS -> Integer
scoreMatch Rock Rock = 3
scoreMatch Rock Paper = 6
scoreMatch Rock Scissors = 0
scoreMatch Paper Rock = 0
scoreMatch Paper Paper = 3
scoreMatch Paper Scissors = 6
scoreMatch Scissors Rock = 6
scoreMatch Scissors Paper = 0
scoreMatch Scissors Scissors = 3

scoreTotal :: RPS -> RPS -> Integer
scoreTotal them me = scoreShape me + scoreMatch them me

data Result = Lose | Draw | Win

toShape :: RPS -> Result -> RPS
toShape s Draw = s
toShape Rock Lose = Scissors
toShape Rock Win = Paper
toShape Paper Lose = Rock
toShape Paper Win = Scissors
toShape Scissors Lose = Paper
toShape Scissors Win = Rock

strToRPS :: String -> RPS
strToRPS "A" = Rock
strToRPS "B" = Paper
strToRPS "C" = Scissors
strToRPS "X" = Rock
strToRPS "Y" = Paper
strToRPS "Z" = Scissors
strToRPS str = error str

strToResult :: String -> Result
strToResult "X" = Lose
strToResult "Y" = Draw
strToResult "Z" = Win
strToResult str = error str

parseGuide :: String -> [(RPS, RPS)]
parseGuide = map (head2 . map strToRPS . words) . lines

parseGuide' :: String -> [(RPS, Result)]
parseGuide' = map (bimap strToRPS strToResult
                   . head2
                   . words) . lines

head2 :: [a] -> (a, a)
head2 xs = (head xs, (head . tail) xs)
