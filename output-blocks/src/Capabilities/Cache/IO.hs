{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Cache.

module Capabilities.Cache.IO () where

import qualified Data.ByteString                  as BS (readFile, writeFile)

import Capabilities.Cache               (MonadCache (..))

import System.Directory                 (doesFileExist)

instance MonadCache IO where
  appendCollisionFile = appendFile
  doesCacheExist = doesFileExist
  readShowFile = BS.readFile
  writeShowFile = BS.writeFile
