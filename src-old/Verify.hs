{-# LANGUAGE AllowAmbiguousTypes #-}

module Raehik.Verify where

import Data.Functor.Identity
import Data.Functor.Const

import Raehik.Hash
import Data.ByteString qualified as B

class Verify f a where
    verify :: f a -> a -> Maybe String

-- | Compare equality.
instance Eq a => Verify Identity a where
    verify ia a = if runIdentity ia == a then Nothing else Just "not equal"

-- | Do nothing.
instance Verify (Const ()) a where
    verify (Const ()) = const Nothing

-- | Hash and compare to digest.
instance Hash h => Verify (Digest h) B.ByteString where
    verify digest bs =
        if hash @h bs /= digest then Nothing else Just "hashes not equal"

{-
class Verify f where
    type VerifyC f a :: Constraint
    verify :: VerifyC f a => f a -> a -> Maybe String

instance Verify Identity where
    type VerifyC Identity a = Eq a
    verify fa a = if runIdentity fa == a then Nothing else Just "not equal"
-}
