#!/usr/bin/env runghc

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Exception
import System.Environment
import System.Exit
import Text.Read

import Utils

import qualified Day01

main :: IO ()
main = do
  (day, part, file) <- parseArgs
  case getExec day part of
    Just exec -> do
      contents <- readFile file
        `catch` \(e :: SomeException) ->
                  putStrLnErr "Could not read input file"
                    >> printErr e
                    >> exitWith (ExitFailure 3)
      exec contents
    _ -> putStrLnErr "Unknown day/part" >> exitWith (ExitFailure 2)


getExec :: String -> String -> Maybe (String -> IO ())
getExec d p = case (readMaybe d, readMaybe p) of
  (Just d', Just p') -> (at' (d'-1) >=> at' (p'-1)) execs
  _ -> Nothing
  where execs :: [[String -> IO ()]]
        execs = [Day01.runs]

parseArgs :: IO (String, String, String)
parseArgs = do
  args <- getArgs
  case args of
    (d : p : f : _) -> return (d,p,f)
    _ -> putStrLnErr "Missing arguments" >> exitWith (ExitFailure 1)
