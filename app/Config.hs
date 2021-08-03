{-# LANGUAGE TemplateHaskell #-}

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

makeClassy ''TGFlowchartCfg
makeClassy ''TGSCPCfg
makeClassy ''TGSL01Cfg
makeClassy ''CfgBinJSON
makeClassy ''CfgBinIO

instance HasCfgBinIO CfgBinJSON where
    cfgBinIO = cfgBinJSONCfgBinIO

instance HasCfgBinJSON TGFlowchartCfg where
    cfgBinJSON = tgFlowchartCfgBinJSON

instance HasCfgBinIO TGFlowchartCfg where
    cfgBinIO = cfgBinJSON . cfgBinIO
