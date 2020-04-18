{-# LANGUAGE DeriveGeneric #-}

module GenericFeed.Binary
 ( encode
 , decode
 , writeCacheFile
 , readCacheFile
 )
where

import Prelude hiding (readFile, writeFile, length)

import Data.ByteString.Lazy
import qualified Data.Binary as B
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Control.Monad ((>=>))
import Control.Exception (try)

import GenericFeed

newtype Container = Container { getContainer :: CacheFile }
  deriving (Generic)

instance Binary Container

encode :: CacheFile -> ByteString
encode = B.encode . Container

decode :: ByteString -> CacheFile
decode = getContainer . B.decode

-- Read CacheFile from a file and make sure the file is closed
readCacheFile :: FilePath -> IO CacheFile
readCacheFile f = do
  let readForSure = readFile >=> (\s -> seq (length s) $ pure s)
  es <- (try :: IO ByteString -> IO (Either IOError ByteString)) $ readForSure f
  case es of
    Left _ -> pure []
    Right s -> pure $ decode s

writeCacheFile :: FilePath -> CacheFile -> IO ()
writeCacheFile f = writeFile f . encode
