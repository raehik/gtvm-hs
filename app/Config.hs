{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens.TH

-- | Shared configuration for tools that serialize between binary and JSON.
data CfgBinaryJSON = CfgBinaryJSON
  { _cfgBinaryJSONDirection           :: ActionDirection
  , _cfgBinaryJSONFilepath            :: FilePath
  , _cfgBinaryJSONOutFilepath         :: Maybe FilePath
  , _cfgBinaryJSONAllowBinaryOnStdout :: Bool
  , _cfgBinaryJSONPrettify            :: Bool
  } deriving (Eq, Show)

data ToolGroup
  = TGFlowchart TGFlowchartCfg
  | TGSCP TGSCPCfg
    deriving (Eq, Show)

data TGFlowchartCfg = TGFlowchartCfg
  { _tgFlowchartCfgBinaryJSON :: CfgBinaryJSON
  , _tgFlowchartCfgType       :: CfgFlowchartType
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
  { _tgSCPCfgBinaryJSON :: CfgBinaryJSON
  } deriving (Eq, Show)

makeLenses ''TGFlowchartCfg

class HasCfgBinaryJSON a where
    getCfgBinaryJSON :: a -> CfgBinaryJSON

instance HasCfgBinaryJSON CfgBinaryJSON where
    getCfgBinaryJSON = id
instance HasCfgBinaryJSON TGFlowchartCfg where
    getCfgBinaryJSON = _tgFlowchartCfgBinaryJSON
