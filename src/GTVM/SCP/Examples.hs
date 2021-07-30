{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP.Examples where

import           GTVM.SCP
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Builder as BB
import           Data.Word

type Builder = BB.Builder

build :: Builder -> Bytes
build = BL.toStrict . BB.toLazyByteString

ex1 :: Bytes
ex1 = build $
       w8 0x05
    <> w8 0x00
    <> w8s [ 0x00, 0x00, 0x00, 0x00 ]
    <> utf8 str1
    <> utf8 str2
    <> w8s [ 0xFF, 0x00, 0x00, 0x00 ]

ex00120zzz0 :: Bytes
ex00120zzz0 = build $
       w8s [ 0x10, 0x01, 0x02, 0x04 ]
    <> w8 0x01 <> utf8 "bg002" <> w8s [ 0x00, 0x04, 0x1F ]
    <> w8s [ 0x05, 0x10, 0x11, 0x00, 0x00, 0x00 ]
    <> utf8 str00120zzz0_1
    <> utf8 str00120zzz0_2
    <> w8s [ 0x00, 0x00, 0x00, 0x00 ]

ex00120zzz0FirstTextboxBS :: Bytes
ex00120zzz0FirstTextboxBS = build $
       w8 0x05 <> w8 0x10 <> w32LE 0x11
    <> utf8 str00120zzz0FirstTextbox
    <> utf8 "rei_K1_00001"
    <> w32LE 0x00

--------------------------------------------------------------------------------

str1, str2 :: String
str1 = "今日"
str2 = "today"

str00120zzz0_1 :: String
str00120zzz0_1 = "多田万里などなど"
str00120zzz0_2 :: String
str00120zzz0_2 = "rei_K1_00001"
str00120zzz0FirstTextbox :: String
str00120zzz0FirstTextbox = "多田万里が、大慌てでドライヤーをかけている。"

--------------------------------------------------------------------------------

w8 :: Word8 -> BB.Builder
w8 = BB.word8

w32LE:: Word32 -> Builder
w32LE = BB.word32LE

w8s :: [Word8] -> Builder
w8s = mconcat . (map w8)

-- | Includes nullterm byte.
utf8 :: String -> Builder
utf8 s = BB.stringUtf8 s <> w8 0x00
