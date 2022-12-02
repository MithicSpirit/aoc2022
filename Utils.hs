module Utils where

import System.IO

head' :: [a] -> Maybe a
head' (x : _) = Just x
head' _ = Nothing

tail' :: [a] -> Maybe [a]
tail' (_ : xs) = Just xs
tail' _ = Nothing

at' :: Int -> [a] -> Maybe a
at' _ []     = Nothing
at' 0 (x:_)  = Just x
at' n (_:xs) | n < 0     = Nothing
             | otherwise = at' (n-1) xs


split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split check l@(x:xs) =
  if check x then split check xs else
    let heads = takeWhile (not . check) l in
      let tails = dropWhile (not . check) l in
        heads : split check tails


printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

printErr :: Show a => a -> IO ()
printErr = putStrLnErr . show
