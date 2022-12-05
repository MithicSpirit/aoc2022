#!/usr/bin/env runghc

module Day05 where

import Data.Bifunctor
import Data.List


main :: IO ()
main = do
  contents <- readFile "day05/input"
  print $ headsAfterMove contents
  print $ headsAfterMoveNoReverse contents


headsAfterMove :: String -> String
headsAfterMove = map head . uncurry doMoves . parse
  where doMoves :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
        doMoves = foldl (flip move)

headsAfterMoveNoReverse :: String -> String
headsAfterMoveNoReverse = map head . uncurry doMoves . parse
  where doMoves :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
        doMoves = foldl (flip moveNoReverse)


move :: (Int, Int, Int) -> [[Char]] -> [[Char]]
move (qty, fromInd, toInd) input = if fromInd < toInd then move1 else move2
  where move1 = start ++ (from' : middle) ++ (to' : end)
          where start = take (fromInd - 1) input
                middle = take (toInd - fromInd - 1) $ drop fromInd input
                end = drop toInd input
                -- Actual important lists
                from = input !! (fromInd - 1)
                to = input !! (toInd - 1)
                -- Make the move
                removed = take qty from
                from' = drop qty from
                to' = reverse removed ++ to
        move2 = start ++ (to' : middle) ++ (from' : end)
          where start = take (toInd - 1) input
                middle = take (fromInd - toInd - 1) $ drop toInd input
                end = drop fromInd input
                -- Actual important lists
                from = input !! (fromInd - 1)
                to = input !! (toInd - 1)
                -- Make the move
                removed = take qty from
                from' = drop qty from
                to' = reverse removed ++ to

moveNoReverse :: (Int, Int, Int) -> [[Char]] -> [[Char]]
moveNoReverse (qty, fromInd, toInd) input = if fromInd < toInd
                                              then move1 else move2
  where move1 = start ++ (from' : middle) ++ (to' : end)
          where start = take (fromInd - 1) input
                middle = take (toInd - fromInd - 1) $ drop fromInd input
                end = drop toInd input
                -- Actual important lists
                from = input !! (fromInd - 1)
                to = input !! (toInd - 1)
                -- Make the move
                removed = take qty from
                from' = drop qty from
                to' = removed ++ to
        move2 = start ++ (to' : middle) ++ (from' : end)
          where start = take (toInd - 1) input
                middle = take (fromInd - toInd - 1) $ drop toInd input
                end = drop fromInd input
                -- Actual important lists
                from = input !! (fromInd - 1)
                to = input !! (toInd - 1)
                -- Make the move
                removed = take qty from
                from' = drop qty from
                to' = removed ++ to

parse :: String -> ([[Char]], [(Int, Int, Int)])
parse = bimap (parseCrates . dropEnd 1) (parseMoves . drop 1)
  . break (== "") . lines

parseCrates :: [String] -> [[Char]]
parseCrates = map (dropWhile (== ' ')) . transpose . map aux
  where aux :: String -> [Char]
        aux = map (!! 1) . chunkOf 4

-- quantity, from, to
parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves = map (\line -> let inWords = words line in
                     (read (inWords !! 1)
                     , read (inWords !! 3)
                     , read (inWords !! 5)))


chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = headN : chunkOf n tailN
  where (headN, tailN) = splitAt n xs

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse
