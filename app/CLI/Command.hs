-- | Common stuff for commands.
module CLI.Command where

import           Config
import           Options.Applicative
import           GHC.Generics

data Cmd a = Cmd
  { cmdName   :: String
  , cmdDesc   :: String
  , cmdParser :: Parser a
  } deriving (Generic)

pCStreamPair :: String -> String -> Parser CStreamPair
pCStreamPair fileModMeta fileMod = CStreamPair <$> pCSIn <*> pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pCSOut   = pFileOpt <|> pure CStreamStd
    pFileArg = CStreamFile <$> strArgument (metavar' <> help ("Input "<>fileMod<>" file"))
    pFileOpt = CStreamFile <$> strOption (metavar' <> long "out-file" <> short 'o' <> help ("Output "<>fileMod<>" file (stdout if not present)"))
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")
    metavar' :: HasMetavar f => Mod f a
    metavar' = metavar $ fileModMeta <> "_FILE"

pFileIn :: String -> String -> Parser FilePath
pFileIn meta noun = strArgument (metavar meta <> help ("Input "<>noun))
