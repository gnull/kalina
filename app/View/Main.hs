module Main where

import System.Environment

import GenericFeed

main :: IO ()
main = do
  [cFile] <- getArgs
  feeds <- (read :: String -> [GenericFeed])<$> readFile cFile
  mapM_ showGenericFeed feeds
