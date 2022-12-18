#!/usr/bin/env runghc

module Day10 where


main :: IO ()
main = do
  contents <- readFile "day10/input"
  print $ signalSum contents
  putStrLn $ draw contents


signalSum :: String -> Int
signalSum = parse .> simul .> zipWith (*) [1..]
            .> flip atMultiple [19,59,99,139,179,219] .> sum

draw :: String -> String
draw = parse .> simul .> chunksOf 40 .> map (zipWith (within 1) [0..])
       .> toDrawing


parse :: String -> [Instruction]
parse = lines .> map readInstruction

data Instruction = Noop | Addx Int

readInstruction :: String -> Instruction
readInstruction ('n':'o':'o':'p':_) = Noop
readInstruction addx = Addx $ read $ words addx !! 1

simul :: [Instruction] -> [Int]
simul = reverse . aux [] 1
  where aux :: [Int] -> Int -> [Instruction] -> [Int]
        aux acc x [] = x:acc
        aux acc x (i:is) = case i of
          Noop -> aux (x:acc) x is
          Addx n -> aux (x:x:acc) (x+n) is

toDrawing :: [[Bool]] -> String
toDrawing = map (map (\x -> if x then '█' else ' ')) .> unlines


(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
infixr 9 .>

atMultiple :: [a] -> [Int] -> [a]
atMultiple xs = map (xs !!)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = headN : chunksOf n tailN
  where (headN, tailN) = splitAt n xs

within :: Int -> Int -> Int -> Bool
within n a b = abs (a-b) <= n
