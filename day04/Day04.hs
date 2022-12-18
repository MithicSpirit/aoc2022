#!/usr/bin/env runghc

module Main where

import Data.List
import Data.Bifunctor


main :: IO ()
main = do
  contents <- readFile "day04/input"
  print $ countContained contents
  print $ countOverlapping contents


countContained :: String -> Int
countContained = sum . map (fromEnum . uncurry contained . parseLine) . lines

countOverlapping :: String -> Int
countOverlapping = sum . map (fromEnum . uncurry overlap . parseLine) . lines


contained :: (Int,Int) -> (Int,Int) -> Bool
contained a b = aux a b || aux b a
  where aux :: (Int,Int) -> (Int,Int) -> Bool
        aux (s1,e1) (s2,e2) = s1 <= s2 && e1 >= e2

overlap :: (Int,Int) -> (Int,Int) -> Bool
overlap a b = aux a b || aux b a
  where aux :: (Int,Int) -> (Int,Int) -> Bool
        aux (s1,e1) (s2,e2) = s1 >= s2 && s1 <= e2

parseLine :: String -> ((Int,Int), (Int,Int))
parseLine = bimap2 (bimap2 read) . bimap2 (splitOnce '-') . splitOnce ','


splitOnce :: Eq a => a -> [a] -> ([a],[a])
splitOnce x xs = maybe (xs,[]) (second tail . flip splitAt xs) pt
  where pt :: Maybe Int
        pt = elemIndex x xs

bimap2 :: Bifunctor p => (c -> d) -> p c c -> p d d
bimap2 fn = bimap fn fn
