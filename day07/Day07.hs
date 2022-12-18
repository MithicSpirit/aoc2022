#!/usr/bin/env runghc

module Main where

import Data.Maybe
import Data.Bifunctor
import Data.Function
import Control.Monad

main :: IO ()
main = do
  contents <- readFile "day07/input"
  print $ totAtMost100000 contents
  print $ smallestDeleteFor30000000Of70000000 contents


totAtMost100000 :: String -> Int
totAtMost100000 = sum . filter (<= 100000) . dirSizes . parse

smallestDeleteFor30000000Of70000000 :: String -> Int
smallestDeleteFor30000000Of70000000 input =
  let sizes = parse input & dirSizes
  in filter (>= 30000000 - (70000000 - maximum sizes)) sizes & minimum


parse :: String -> Dir
parse = fst . parseCmds emptyDir . groupAt (startsWith '$') . tail . lines
  where
    parseCmds :: Dir -> [[String]] -> (Dir, [[String]])
    parseCmds curr [] = (curr, [])
    parseCmds curr (c:cs) = case commandData c of
      CdOut -> (curr, cs)
      CdIn -> let (dir, left) = parseCmds emptyDir cs
              in parseCmds (dirAddDir curr dir) left
      Ls fs -> parseCmds (dirSetFiles curr fs) cs

data Command = CdOut | CdIn | Ls [File]
commandData :: [String] -> Command
commandData ("$ cd ..":_) = CdOut
commandData (('$':' ':'c':'d':' ':_):_) = CdIn
commandData (_:files) = Ls $ mapMaybe entryToFile files
commandData _ = Ls []

type File = Int
data Dir = Dir [File] [Dir]

emptyDir :: Dir
emptyDir = Dir [] []

dirSetFiles :: Dir -> [File] -> Dir
dirSetFiles (Dir _ dirs) fs = Dir fs dirs

dirAddDir :: Dir -> Dir -> Dir
dirAddDir (Dir fs dirs) d = Dir fs (d:dirs)

entryToFile :: String -> Maybe File
entryToFile ('d':'i':'r':' ':_) = Nothing
entryToFile l = Just $ read $ head $ words l

dirSizes :: Dir -> [Int]
dirSizes = uncurry (flip (:)) . getSizes
  where getSizes :: Dir -> ([Int], Int)
        getSizes (Dir files dirs) =
          let (recursive,direct) = map getSizes dirs & unzip & first join
          in (recursive ++ direct, sum direct + sum files)


startsWith :: Eq a => a -> [a] -> Bool
startsWith x0 (x:_) = x0 == x
startsWith _ [] = False

groupAt :: (a -> Bool) -> [a] -> [[a]]
groupAt _ [] = []
groupAt f (x:xs) = break f xs & bimap (x:) (groupAt f) & uncurry (:)

dupe :: a -> (a,a)
dupe x = (x,x)
