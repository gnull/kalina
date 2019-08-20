module Main where

import System.Environment

import GenericFeed

main :: IO ()
main = do
  [cFile] <- getArgs
  feeds <- read <$> readFile cFile
  putStrLn $ unlines $ map showGenericFeed feeds
