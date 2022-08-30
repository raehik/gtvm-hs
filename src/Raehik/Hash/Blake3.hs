module Raehik.Hash.Blake3 where

import Raehik.Hash.Internal

import Data.ByteString qualified as B
import BLAKE3 qualified as B3
import Data.ByteArray qualified as BA

data Blake3
instance Hash Blake3 where
    type HashFuncLabel Blake3 = "b3"
    hash' bs = B.pack $ BA.unpack $ B3.hash @B3.DEFAULT_DIGEST_LEN [bs]
