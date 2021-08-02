{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where

import           Control.Lens.TH

-- | Config for tools that serialize between binary and JSON.
data CfgBinJSON = CfgBinJSON
  { _cfgBinJSONCfgBinIO :: CfgBinIO
  , _cfgBinJSONPrettify :: Bool
  } deriving (Eq, Show)

-- | Config for tools that do IO with binary data.
data CfgBinIO = CfgBinIO
  { _cfgBinIODirection           :: ActionDirection
  , _cfgBinIOFilepath            :: FilePath
  , _cfgBinIOOutFilepath         :: Maybe FilePath
  , _cfgBinIOAllowBinaryOnStdout :: Bool
  } deriving (Eq, Show)

data ToolGroup
  = TGFlowchart TGFlowchartCfg
  | TGSCP TGSCPCfg
  | TGSL01 TGSL01Cfg
    deriving (Eq, Show)

data TGFlowchartCfg = TGFlowchartCfg
  { _tgFlowchartCfgBinJSON :: CfgBinJSON
  , _tgFlowchartCfgType    :: CfgFlowchartType
  } deriving (Eq, Show)

data CfgFlowchartType
  = CfgFlowchartTypeParse
  | CfgFlowchartTypeLex
    deriving (Eq, Show)

data ActionDirection
  = ActionDirectionEncode
  | ActionDirectionDecode
    deriving (Eq, Show)

data TGSCPCfg = TGSCPCfg
  { _tgSCPCfgBinJSON :: CfgBinJSON
  } deriving (Eq, Show)

data TGSL01Cfg = TGSL01Cfg
  { _tgSL01CfgBinIO :: CfgBinIO
  } deriving (Eq, Show)

makeLenses ''TGFlowchartCfg
makeLenses ''TGSCPCfg
makeLenses ''TGSL01Cfg
makeLenses ''CfgBinJSON
makeLenses ''CfgBinIO

class HasCfgBinIO a where
    getCfgBinIO :: a -> CfgBinIO

instance HasCfgBinIO CfgBinIO where
    getCfgBinIO = id

class HasCfgBinJSON a where
    getCfgBinJSON :: a -> CfgBinJSON

newtype Wrap a = Wrap { unwrap :: a }
instance HasCfgBinJSON a => HasCfgBinIO (Wrap a) where
    getCfgBinIO = _cfgBinJSONCfgBinIO . getCfgBinJSON . unwrap

instance HasCfgBinJSON CfgBinJSON where
    getCfgBinJSON = id
instance HasCfgBinJSON TGFlowchartCfg where
    getCfgBinJSON = _tgFlowchartCfgBinJSON

deriving via (Wrap CfgBinJSON) instance HasCfgBinIO CfgBinJSON
deriving via (Wrap TGFlowchartCfg) instance HasCfgBinIO TGFlowchartCfg
