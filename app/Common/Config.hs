module Common.Config where

import GHC.Generics
import GHC.TypeLits

-- | "Single file" stream decorated with type information.
--
-- The rationale is that the representation is always the same, but we want to
-- treat in streams differently to out streams in the CLI help.
data Stream (d :: StreamDirection) (s :: Symbol)
  = StreamFile' (StreamFile d s)
  | StreamStd
    deriving (Eq, Show, Generic)

newtype StreamFile (d :: StreamDirection) (s :: Symbol)
  = StreamFile { streamFilePath :: FilePath }
    deriving (Eq, Show, Generic)

-- | Stream direction (in or out).
data StreamDirection = StreamIn | StreamOut deriving (Eq, Show, Generic)

data PrintBin
  = PrintBin
  | NoPrintBin
    deriving (Eq, Show, Generic)
