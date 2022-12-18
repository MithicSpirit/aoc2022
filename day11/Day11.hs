#!/usr/bin/env runghc

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List
import Data.Char
import Data.Function
import Data.Bifunctor

main :: IO ()
main = do
  contents <- readFile "day11/input"
  putStrLn $ monkeyBizUnworry contents
  putStrLn $ monkeyBizNoUnworry contents


monkeyBizUnworry :: String -> String
monkeyBizUnworry = parse .> (\(ms,red) -> simulate 3 red 20 ms) .> unzip .> snd
                   .> sort .> reverse .> take 2 .> product .> show

monkeyBizNoUnworry :: String -> String
monkeyBizNoUnworry = parse .> (\(ms,red) -> simulate 1 red 10000 ms) .> unzip
                     .> snd .> sort .> reverse .> take 2 .> product .> show


parse :: String -> ([Monkey], Int)
parse = lines .> groupAtRm "" .> map parseMonkey .> unzip .> second (foldr1 lcm)
  where parseMonkey :: [String] -> (Monkey, Int)
        parseMonkey (_:items:op:test:ifT:ifF:_) =
          (Monkey items' op' test', check)
          where items' :: [Item]
                items' = words items & drop 2
                  & map (Item . read . filter isNumber)
                op' :: Operation
                op' = words op & drop 4 & \case
                  ["*", "old"] -> (^2)
                  ["*", val] -> (* (Item . read) val)
                  ["+", val] -> (+ (Item . read) val)
                  _ -> error ("Invalid operation " ++ op)
                check :: Int
                check = words test !! 3 & read
                test' :: Test
                test' worry = if worry `mod` Item check == 0 then ifT'
                                                             else ifF'
                ifT' :: Int
                ifT' = words ifT !! 5 & read
                ifF' :: Int
                ifF' = words ifF !! 5 & read
        parseMonkey e = error ("Couldn't parse:\n" ++ unlines e)

data Monkey = Monkey { items :: [Item]
                     , op :: Operation
                     , t :: Test
                     }
newtype Item = Item { worry :: Int }
  deriving (Eq, Ord, Enum, Num, Integral, Real)
type Operation = Item -> Item
type Test = Item -> Int

instance Show Monkey where
  show (Monkey is _ _) = "(Monkey " ++ show is ++ ")"
instance Show Item where
  show (Item i) = show i

step :: Int -> Int -> [(Monkey,Int)] -> [(Monkey,Int)]
step divWorry modWorry monkeys = foldl (flip aux) monkeys [0..length monkeys-1]
  where aux :: Int -> [(Monkey,Int)] -> [(Monkey,Int)]
        aux n ms =
          let m = ms !! n & fst
              in case m of
                  (Monkey [] _ _) -> ms
                  (Monkey is op t) ->
                    map (transform op t) is & executeMoves ms
                    & flip (transfAt $ bimap (const $ Monkey [] op t)
                                             (+ length is)) n

        transform :: Operation -> Test -> Item -> (Int,Item)
        transform op t worry = let worry' = (op worry
                                             `div` Item divWorry)
                                             `mod` Item modWorry
                                   in (t worry', worry')

        executeMoves :: [(Monkey,Int)] -> [(Int,Item)] -> [(Monkey,Int)]
        executeMoves ms [] = ms
        executeMoves ms ((n,w):is) = executeMoves ms' is
          where ms' :: [(Monkey,Int)]
                ms' = transfAt (first $ flip appendMonkey w) ms n

simulate :: Int -> Int -> Int -> [Monkey] -> [(Monkey,Int)]
simulate divWorry modWorry n ms = aux n (zip ms (repeat 0))
  where aux :: Int -> [(Monkey,Int)] -> [(Monkey,Int)]
        aux 0 ms = ms
        aux n ms = aux (n-1) (step divWorry modWorry ms)

appendMonkey :: Monkey -> Item -> Monkey
appendMonkey (Monkey is op t) i = Monkey (is ++ [i]) op t


(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixr 9 .>

groupAtRm :: forall a. Eq a => a -> [a] -> [[a]]
groupAtRm _ [] = []
groupAtRm x xs = maybe [xs] aux (elemIndex x xs)
  where aux :: Int -> [[a]]
        aux n = let (h,t) = splitAt n xs in h : groupAtRm x (tail t)

setAt :: Show a => a -> [a] -> Int -> [a]
setAt x xs n = let (h,t) = splitAt n xs in h ++ [x] ++ tail t

transfAt :: Show a => (a -> a) -> [a] -> Int -> [a]
transfAt f xs n = setAt (f $ xs !! n) xs n
