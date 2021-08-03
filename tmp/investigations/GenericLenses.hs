{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GenericLenses where

import           GHC.Generics
import           Data.Generics.Product.Typed
import           Control.Lens

data TOuter = TOuter TMiddle
  deriving (Eq, Show, Generic)
data TMiddle = TMiddle TInner
  deriving (Eq, Show, Generic)
data TInner = TInner
  deriving (Eq, Show, Generic)

dataTOuter = TOuter $ TMiddle $ TInner

--xxxTest :: TOuter -> TInner
--xxxTest = view typed

xxxTest' :: HasType TInner a => a -> TInner
xxxTest' = view typed
