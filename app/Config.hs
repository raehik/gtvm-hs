{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens.TH

data ToolGroup
  = TGFlowchart TGFlowchartCfg
    deriving (Eq, Show)

data TGFlowchartCfg = TGFlowchartCfg
  { _tgFlowchartCfgDirection :: ActionDirection
  , _tgFlowchartCfgType      :: CfgFlowchartType
  , _tgFlowchartCfgFilepath  :: FilePath
  } deriving (Eq, Show)

data ActionDirection
  = ActionDirectionEncode
  | ActionDirectionDecode
    deriving (Eq, Show)

data CfgFlowchartType
  = CfgFlowchartTypeParse
  | CfgFlowchartTypeLex
    deriving (Eq, Show)

makeLenses ''TGFlowchartCfg
