{-# LANGUAGE AllowAmbiguousTypes #-}

module Raehik.Hash
  ( module Raehik.Hash.Internal
  , module Raehik.Hash
  ) where

import Raehik.Hash.Internal

import Data.ByteString qualified as B

-- | Return the output digest of hashing the given bytestring using the
--   requested hash function.
hash :: forall h. Hash h => B.ByteString -> Digest h B.ByteString
hash = Digest . hash' @h
