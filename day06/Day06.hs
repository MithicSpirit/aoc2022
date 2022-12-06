#!/usr/bin/env runghc

module Day06 where

import Data.Bifunctor
import Data.List


main :: IO ()
main = do
  contents <- readFile "day06/input"
  print $ startOfPacket contents
  print $ startOfMessage contents


startOfPacket :: String -> Int
startOfPacket = firstUnique 4

startOfMessage :: String -> Int
startOfMessage = firstUnique 14


firstUnique :: Int -> String -> Int
firstUnique n = maybe 0 fst . find (allUnique . snd)
                . zip [n..] . chunksOfCumulative n

chunksOfCumulative :: Int -> [a] -> [[a]]
chunksOfCumulative _ [] = []
chunksOfCumulative n l@(_:xs)
  | length l < n = []
  | otherwise    = take n l : chunksOfCumulative n xs


allUnique :: Eq a => [a] -> Bool
allUnique xs = xs == nub xs
