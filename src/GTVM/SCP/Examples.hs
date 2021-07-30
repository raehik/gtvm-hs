{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP.Examples where

import           GTVM.SCP
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
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
    <> utf8cstr str00120zzz0_1
    <> utf8cstr str00120zzz0_2
    <> w8s [ 0x00, 0x00, 0x00, 0x00 ]

ex00120zzz0FirstTextboxBS :: Bytes
ex00120zzz0FirstTextboxBS = build $
       w8 0x05 <> w8 0x10 <> w32LE 0x11
    <> utf8cstr str00120zzz0FirstTextbox
    <> utf8cstr "rei_K1_00001"
    <> w32LE 0x00

ex00120zzz0FirstTextboxAltOptsBS :: Bytes
ex00120zzz0FirstTextboxAltOptsBS = build $
       w8 0x05 <> w8 0x10 <> w32BE 0x11
    <> utf8pascal str00120zzz0FirstTextbox
    <> utf8pascal "rei_K1_00001"
    <> w32BE 0x00

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

w32BE:: Word32 -> Builder
w32BE = BB.word32BE

w8s :: [Word8] -> Builder
w8s = mconcat . (map w8)

esBS :: Bytes -> Builder
esBS = BB.byteString

-- | Includes nullterm byte.
utf8cstr :: String -> Builder
utf8cstr s = utf8 s <> w8 0x00

-- | Includes header byte.
--
-- Stupid due to types.
utf8pascal :: String -> Builder
utf8pascal s =
    let bytes  = BSC.pack s
        len = BS.length bytes
        lenW8 = fromIntegral len :: Word8 -- TODO unsafe
     in w8 lenW8 <> esBS bytes

utf8 :: String -> Builder
utf8 s = BB.stringUtf8 s
