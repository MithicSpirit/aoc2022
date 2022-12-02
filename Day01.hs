module Day01 where

import Data.List

import Utils

runs :: [String -> IO ()]
runs = [ print . getMaxSum . parse
       , print . get3MaxSum . parse
       ]

getMaxSum :: [[Integer]] -> Integer
getMaxSum = maximum . map sum

get3MaxSum :: [[Integer]] -> Integer
get3MaxSum = (*) (-1) . sum . take 3 . sort . map ((*) (-1) . sum)

parse :: String -> [[Integer]]
parse = map (map read) . split (== "") . lines
