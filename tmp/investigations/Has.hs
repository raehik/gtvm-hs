{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Has where

import           Data.Functor.Identity  ( Identity(..) )
import           Control.Applicative    ( Const(Const, getConst) )

type Lens' t a = forall f. Functor f => (a -> f a) -> t -> f t

class Has a t where
    {-# MINIMAL getter, modifier | hasLens #-}
    getter :: t -> a
    getter = getConst . hasLens Const

    modifier :: (a -> a) -> t -> t
    modifier f t = runIdentity (hasLens (Identity . f) t)

    hasLens :: Lens' t a
    hasLens afa t = (\a -> modifier (const a) t) <$> afa (getter t)

instance Has a a where
    getter = id
    modifier = id
