#!/usr/bin/env runghc

module Main where

import Data.Char
import Data.List
import Control.Monad
import Data.Bifunctor


main :: IO ()
main = do
  contents <- readFile "day08/input"
  print $ countVisible contents
  print $ bestScore contents

countVisible :: String -> Int
countVisible = sum . map (length . filter id) . whichVisible . parse

bestScore :: String -> Int
bestScore = maximum . scenicScores . parse


parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

whichVisible :: [[Int]] -> [[Bool]]
whichVisible trees = foldl1 merge2 $ map ($ trees)
  [ map visibleFromLeft
  , transpose . map visibleFromLeft . transpose
  , map (reverse . visibleFromLeft . reverse)
  , transpose . map (reverse . visibleFromLeft . reverse) . transpose
  ]
  where visibleFromLeft :: [Int] -> [Bool]
        visibleFromLeft = aux (-1)
          where aux :: Int -> [Int] -> [Bool]
                aux _ [] = []
                aux n (x:xs) | x > n     = True  : aux x xs
                            | otherwise = False : aux n xs
        merge2 :: [[Bool]] -> [[Bool]] -> [[Bool]]
        merge2 = zipWith merge

        merge :: [Bool] -> [Bool] -> [Bool]
        merge = zipWith (||)


scenicScores :: [[Int]] -> [Int]
scenicScores trees = map calculateScore (join (getPositionMat trees))
  where calculateScore :: Position -> Int
        calculateScore p = product $ map (countVisible $ getAt trees p)
                           (tupToList (getRowSplit trees p)
                            ++ tupToList (getColSplit trees p))

        countVisible :: Int -> [Int] -> Int
        countVisible n = length . takeUntilPlus (>= n)

type Position = (Int,Int)  -- row, column

getRowSplit :: [[a]] -> Position -> ([a], [a])
getRowSplit mat (r,c) = bimap reverse tail $ splitAt c $ mat !! r
getColSplit :: [[a]] -> Position -> ([a], [a])
getColSplit mat (r,c) = bimap reverse tail $ splitAt r $ transpose mat !! c

getAt :: [[a]] -> Position -> a
getAt mat (r,c) = mat !! r !! c

getPositionMat :: [[a]] -> [[Position]]
getPositionMat mat = zipWith zip (transpose $ map aux (transpose mat))
                                 (map aux mat)
  where aux :: [a] -> [Int]
        aux xs = [0..length xs - 1]


tupToList :: (a,a) -> [a]
tupToList (x,y) = [x,y]

takeUntilPlus :: (a -> Bool) -> [a] -> [a]
takeUntilPlus _ [] = []
takeUntilPlus p (x:xs) = if p x then [x] else x : takeUntilPlus p xs
