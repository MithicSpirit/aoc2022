#!/usr/bin/env runghc

module Day01 where

import Data.List

main :: IO ()
main = do
  contents <- readFile "day01/input"
  print $ getMaxSum contents
  print $ get3MaxSum contents

getMaxSum :: String -> Integer
getMaxSum = maximum . map sum . parse

get3MaxSum :: String -> Integer
get3MaxSum = (*) (-1) . sum . take 3 . sort . map ((*) (-1) . sum) . parse

parse :: String -> [[Integer]]
parse = map (map read) . split (== "") . lines

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split check l@(x:xs) =
  if check x then split check xs else
    let heads = takeWhile (not . check) l in
      let tails = dropWhile (not . check) l in
        heads : split check tails
