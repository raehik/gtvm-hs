{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens.TH
import           Control.Monad.Reader

data CfgBinaryProcessing = CfgBinaryProcessing
  { _cfgBinaryProcessingDirection           :: ActionDirection
  , _cfgBinaryProcessingFilepath            :: FilePath
  , _cfgBinaryProcessingOutFilepath         :: Maybe FilePath
  , _cfgBinaryProcessingAllowBinaryOnStdout :: Bool
  } deriving (Eq, Show)

data ToolGroup
  = TGFlowchart TGFlowchartCfg
  | TGSCP TGSCPCfg
    deriving (Eq, Show)

data TGFlowchartCfg = TGFlowchartCfg
  { _tgFlowchartCfgBinaryProcessing :: CfgBinaryProcessing
  , _tgFlowchartCfgType             :: CfgFlowchartType
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
  { _tgSCPCfgBinaryProcessing :: CfgBinaryProcessing
  } deriving (Eq, Show)

makeLenses ''TGFlowchartCfg

class HasCfgBinaryProcessing a where
    getCfgBinaryProcessing :: a -> CfgBinaryProcessing

instance HasCfgBinaryProcessing CfgBinaryProcessing where
    getCfgBinaryProcessing = id
instance HasCfgBinaryProcessing TGFlowchartCfg where
    getCfgBinaryProcessing = _tgFlowchartCfgBinaryProcessing
