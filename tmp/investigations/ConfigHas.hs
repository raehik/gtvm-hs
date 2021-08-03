{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module ConfigHas where

import           Control.Lens
import           Config

class HasCfgBinIO' a where
    lensCfgBinIO :: ALens' a CfgBinIO

instance HasCfgBinIO' CfgBinIO where
    lensCfgBinIO = ($)

class HasCfgBinJSON' a where
    lensCfgBinJSON :: Lens' a CfgBinJSON

instance HasCfgBinJSON' CfgBinJSON where
    lensCfgBinJSON = ($)
instance HasCfgBinJSON' TGFlowchartCfg where
    lensCfgBinJSON = tgFlowchartCfgBinJSON

instance HasCfgBinJSON' a => HasCfgBinIO' (Wrap a) where
    lensCfgBinIO = xxxF . lensCfgBinJSON @a . cfgBinJSONCfgBinIO

xxxF :: forall f a. Functor f => (a -> f a) -> Wrap a -> f (Wrap a)
xxxF f (Wrap a) = Wrap <$> f a

deriving via (Wrap CfgBinJSON) instance HasCfgBinIO' CfgBinJSON
deriving via (Wrap TGFlowchartCfg) instance HasCfgBinIO' TGFlowchartCfg

