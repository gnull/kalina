module Config_ where

data Config = Config
  { queueFetching :: FilePath -> IO ()
  }
