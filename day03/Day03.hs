#!/usr/bin/env runghc

{-# LANGUAGE ScopedTypeVariables #-}

module Day03 where

import Data.Char
import Data.List
import Data.Bifunctor


main :: IO ()
main = do
  contents <- readFile "day03/input"
  print $ common contents
  print $ badges contents


common :: String -> Int
common = sum . map (sum . map prio . uncurry intersect
                    . bimap nub nub . halve) . lines

badges :: String -> Int
badges = sum . map (sum . map prio . foldr1 intersect
                    . map nub) . chunkOf 3 . lines


prio :: Char -> Int
prio a | isLower a = ord a - ord 'a' + 1
       | isUpper a = ord a - ord 'A' + 27

halve :: [a] -> ([a], [a])
halve xs = let (half :: Int) = length xs `div` 2 in splitAt half xs

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = headN : chunkOf n tailN
  where (headN, tailN) = splitAt n xs
