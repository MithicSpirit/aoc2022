#!/usr/bin/env runghc

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Function
import Data.Char
import Data.List
import Control.Monad
import Data.Maybe


main :: IO ()
main = do
  contents <- readFile "day12/input"
  putStrLn $ findPath contents
  putStrLn $ findBest contents


findPath :: String -> String
findPath = parsePart1 .> uncurry3 searchBFS .> fromJust .> length .> show

findBest :: String -> String
findBest input = searchBFSEq map' end 0 & fromJust & length & show
  where map' = parseMap $ lines input
        end = head $ elemPositions 'E' $ lines input


parsePart1 :: String -> (Map, Position, Position)
parsePart1 str = ( parseMap ls
            , head $ elemPositions 'S' ls
            , head $ elemPositions 'E' ls
            )
  where ls = lines str

parseMap :: [String] -> Map
parseMap = map (map toHeight)
  where toHeight :: Char -> Int
        toHeight 'S' = toHeight 'a'
        toHeight 'E' = toHeight 'z'
        toHeight c = ord c - ord 'a'

type Map = [[Int]]
type Position = (Int, Int)

getHeight :: Map -> Position -> Int
getHeight m (x,y) = m !! x !! y

getAdjacent :: Map -> Position -> [Position]
getAdjacent m (x,y) = filter (inBounds m) [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]

inBounds :: Map -> Position -> Bool
inBounds m (x,y) = not (x < 0 || y < 0 || x >= length m || y >= length (m !! x))

searchBFS :: Map -> Position -> Position -> Maybe [Position]
searchBFS map' start target = aux [start] [start] (const (0,0))
  where height :: Position -> Int
        height = getHeight map'

        aux :: [Position] -> [Position] -> (Position -> Position) -> Maybe [Position]
        aux [] _ _ = Nothing
        aux (curr:toVisit) haveVisited getTo
          | curr == target = Just $ walkBack getTo target start
          | otherwise =
            aux (toVisit ++ adjacentNew) (adjacentNew ++ haveVisited) getTo'
            where adjacentNew :: [Position]
                  adjacentNew = getAdjacent map' curr
                                & filter (not . flip elem haveVisited)
                                & filter ((<= height curr + 1) . height)
                  getTo' :: Position -> Position
                  getTo' p = if p `elem` adjacentNew then curr else getTo p


searchBFSEq :: Map -> Position -> Int -> Maybe [Position]
searchBFSEq map' start target = aux [start] [start] (const (0,0))
  where height :: Position -> Int
        height = getHeight map'

        aux :: [Position] -> [Position] -> (Position -> Position) -> Maybe [Position]
        aux [] _ _ = Nothing
        aux (curr:toVisit) haveVisited getTo
          | height curr == target = Just $ walkBack getTo curr start
          | otherwise =
            aux (toVisit ++ adjacentNew) (adjacentNew ++ haveVisited) getTo'
            where adjacentNew :: [Position]
                  adjacentNew = getAdjacent map' curr
                                & filter (not . flip elem haveVisited)
                                & filter ((>= height curr - 1) . height)
                  getTo' :: Position -> Position
                  getTo' p = if p `elem` adjacentNew then curr else getTo p

walkBack :: forall a. Eq a => (a -> a) -> a -> a -> [a]
walkBack rel start end = aux [] start
  where aux :: [a] -> a -> [a]
        aux acc curr | curr == end = acc
                     | otherwise   = aux (curr:acc) (rel curr)


(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixr 9 .>

elemPositions :: forall a. Eq a => a -> [[a]] -> [Position]
elemPositions find = map (elemIndices find) .> zipWith aux [0..] .> join
  where aux :: Int -> [Int] -> [(Int,Int)]
        aux curr = zip (repeat curr)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 fn (a,b,c) = fn a b c

dupe :: a -> (a,a)
dupe x = (x,x)
