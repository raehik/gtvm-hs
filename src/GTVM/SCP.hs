{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TypeOperators      #-}

module GTVM.SCP where

import           Control.Lens               hiding ((:<))
import           Control.Lens.TH
import           Data.Functor.Foldable.TH
import           Data.Fix
import           Control.Comonad.Cofree

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           GHC.Word

import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Streaming.ByteString       as Q

import           Text.Megaparsec

import qualified Data.Binary.Get as Get

import           Control.Monad.Except

{-
class HasSCPRep a where
    scpBytes :: a -> BS.ByteString
-}

-- | A standalone segment of an SCP file. Essentially tokens from a byte lexer.
data SCPSegment
  = SCPSegTextbox Word8 Word32 BS.ByteString BS.ByteString Word32
  | SCPSegUnk03Sprite Word8 BS.ByteString
  | SCPSegUnk10 Word8 Word8 Word8
  | SCPSegUnk08 -- appears empty
  | SCPSegUnk0A Word8
  | SCPSegUnk34 Word32
  | SCPSegUnk44SFX BS.ByteString Word8
  | SCPSegUnk45SFX BS.ByteString Word8
  | SCPSegSFX BS.ByteString Word8
  | SCPSegBg BS.ByteString
    deriving (Eq, Show)
