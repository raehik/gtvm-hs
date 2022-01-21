{-# LANGUAGE AllowAmbiguousTypes #-}

module Common.CLIOptions where

import Common.Config
import GHC.TypeLits hiding ( Mod )
import Options.Applicative hiding ( str )
import GHC.Exts
import Data.Char qualified as Char

streamStdin :: forall s. Stream 'StreamIn s
streamStdin = StreamStd
streamStdout :: forall s. Stream 'StreamOut s
streamStdout = StreamStd

pStreamIn :: forall s. KnownSymbol s => Parser (Stream 'StreamIn s)
pStreamIn = (StreamFile' <$> pStreamFileIn) <|> pStdinOpt
  where
    pStdinOpt = flag' StreamStd $  long "stdin"
                                <> help ("Get "<>sym @s<>" from stdin")

pStreamOut :: forall s. KnownSymbol s => Parser (Stream 'StreamOut s)
pStreamOut = (StreamFile' <$> pStreamFileOut) <|> pure StreamStd
  where
    pStreamFileOut = StreamFile <$> strOption (modFileOut (sym @s))

pStreamFileIn :: forall s. KnownSymbol s => Parser (StreamFile 'StreamIn s)
pStreamFileIn = StreamFile <$> strArgument (modFileIn (sym @s))

pPrintBin :: Parser PrintBin
pPrintBin = flag NoPrintBin PrintBin $  long "print-binary"
                                     <> help "Allow printing binary to stdout"


-- | More succint 'symbolVal' via type application.
sym :: forall s. KnownSymbol s => String
sym = symbolVal' (proxy# :: Proxy# s)

-- | Generate a base 'Mod' for a file type using the given descriptive
--   name (the "type" of input, e.g. file format) and the given direction.
modFile :: HasMetavar f => String -> String -> Mod f a
modFile dirStr descStr =  metavar "FILE"
                       <> help (dirStr<>" "<>descStr')
  where
    descStr' = descStr<>" file"

modFileIn :: HasMetavar f => String -> Mod f a
modFileIn = modFile "Input"

modFileOut :: (HasMetavar f, HasName f) => String -> Mod f a
modFileOut s = modFile "Output" s <> long "out-file" <> short 'o'

metavarify :: String -> String
metavarify = map Char.toUpper . map spaceToUnderscore
  where spaceToUnderscore = \case ' ' -> '_'; ch -> ch
