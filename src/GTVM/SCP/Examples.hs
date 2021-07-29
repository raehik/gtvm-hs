{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP.Examples where

import           GTVM.SCP
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BSB
import           Data.Word

ex1 :: BS.ByteString
ex1 = BL.toStrict $ BSB.toLazyByteString $
       w8 0x05
    <> w8 0x00
    <> w8s [ 0x00, 0x00, 0x00, 0x00 ]
    <> utf8 str1 <> w8 0x00
    <> utf8 str2 <> w8 0x00
    <> w8s [ 0xFF, 0x00, 0x00, 0x00 ]

ex00120zzz0 :: BS.ByteString
ex00120zzz0 = BL.toStrict $ BSB.toLazyByteString $
       w8s [ 0x10, 0x01, 0x02, 0x04 ]
    <> w8 0x01 <> utf8 "bg002" <> w8 0x00 <> w8s [ 0x00, 0x04, 0x1F ]
    <> w8s [ 0x05, 0x10, 0x11, 0x00, 0x00, 0x00 ]
    <> utf8 str00120zzz0_1 <> w8 0x00
    <> utf8 str00120zzz0_2 <> w8 0x00
    <> w8s [ 0x00, 0x00, 0x00, 0x00 ]

--------------------------------------------------------------------------------

str1, str2 :: String
str1 = "今日"
str2 = "today"

str00120zzz0_1 :: String
str00120zzz0_1 = "多田万里などなど"
str00120zzz0_2 :: String
str00120zzz0_2 = "rei_K1_00001"

--------------------------------------------------------------------------------

w8 :: Word8 -> BSB.Builder
w8 = BSB.word8

w8s :: [Word8] -> BSB.Builder
w8s = mconcat . (map w8)

utf8 :: String -> BSB.Builder
utf8 = BSB.stringUtf8
