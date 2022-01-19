{-|
A wrapper on top of "Refined" that encourages using the same type for refined
and unrefined views of the same data. Useful when both views have identical
structure (i.e. the only difference is that whether some invariants are
enforced). For information on refinement types and the underlying concept, see
"Refined".

Say you're already sold on refinement types as a method to easily describe data
more precisely without complicating algorithms or comprising runtime
performance. You work with plain types internally, and refine only when you need
to. This means when working with records with refined types, you necessarily
duplicate the record: one for refined, one for unrefined.

Sometimes this is fine, because you change the data structure between refining
(e.g. you drop some indices that are generatable). Sometimes, it means you have
two records storing the same data, where one has some invariants enforced and
the other doesn't. In the former case, you should have one data type with
'Refined' fields, one without. This module focuses on the latter use case.

The 'WithRefine' newtype enables you to use the same type for both views by
wrapping refined types with a type-level switch that tracks whether the
predicate is currently enforced.

  * The type @t'WithRefine' ''Enforced' p a@ corresponds to @'Refined' p a@: a
    value of type @a@ with an enforced predicate @p@. Also available at
    @'Strong' p a@.
  * The type @t'WithRefine' ''Unenforced' p a@ corresponds to... @a@. It's a
    value of type @a@ tagged with predicate @p@, but the predicate remains
    unenforced. Also available at @'Weak' p a@.

Now internally, you write your code to only transform @'Weak' p a@ values. When,
say, serializing between an external system, you 'enforce' the predicate,
upgrading to a @'Strong' p a@. The upshot compared to plain refined:

  * No need to duplicate types when working with refined types.
  * Your internal code must be aware of your refined types. This can be good and
    bad: more wrapping, but also more type information.

There should be even less runtime impact than refined, because we don't store
anything at value-level (so unrefining is now "free").

-}

{-# LANGUAGE PolyKinds, RoleAnnotations #-}

module Refined.WithRefine
  (
  -- * 'WithRefine' types
    WithRefine, unWithRefine
  , withRefine
  , PredicateStatus(..)
  , Strong, Weak

  -- * Enforcing
  , enforce
  , enforceFail

  -- * Unenforcing
  , unenforce

  -- * Coercing between Refined
  , enforcedToRefined
  , refinedToEnforced

  -- * Unsafe enforcing
  , reallyUnsafeEnforce

  -- * Other definitions
  , WithRefineRep
  ) where

import Refined
import Refined.Unsafe
import Data.Proxy
import Control.DeepSeq ( NFData )
import Data.Hashable ( Hashable )
import GHC.Generics ( Generic )
import Data.Aeson
import Control.Exception

-- | A wrapper over a type @a@ with a predicate @p@, and a type-level "switch"
--   indicating whether the predicate is enforced or not.
--
-- @t'WithRefine' ''Enforced' p a@ is equivalent to 'Refined p a', while
-- @t'WithRefine' ''Unenforced' p a@ is equivalent to @a@. This newtype lets you
-- use the same term-level constructor for both. One could then combine multiple
-- t'WithRefine'-wrapped fields into a single record and parameterize over the
-- predicate status to essentially enable "weak" and "strong" views of the
-- record, while conveniently using the same value-level representation.
newtype WithRefine (ps :: PredicateStatus) p a
  = WithRefine {
      -- | Extracts any predicate-wrapped value. Works regardless of whether the
      --   predicate was enforced or not.
      unWithRefine :: a
  } deriving newtype (Generic, Hashable, NFData, Eq, Ord)
    deriving stock   (Show, Foldable)

deriving stock instance Functor     (WithRefine 'Unenforced p)
deriving stock instance Traversable (WithRefine 'Unenforced p)

instance ToJSON   a => ToJSON   (WithRefine ps          p a) where
    toJSON     = toJSON     . unWithRefine
    toEncoding = toEncoding . unWithRefine

instance FromJSON a => FromJSON (WithRefine 'Unenforced p a) where
    parseJSON = fmap withRefine . parseJSON

instance (FromJSON a, Predicate p a) => FromJSON (WithRefine 'Enforced p a) where
    parseJSON a = parseJSON a >>= enforceFail

type role WithRefine nominal nominal nominal

-- | Wrap a value with any unenforced predicate. This is like tagging your value
--   with the note "this value needs to be checked using the given predicate".
withRefine :: a -> WithRefine 'Unenforced p a
withRefine = WithRefine

-- | Is the associated predicate currently enforced or not?
data PredicateStatus
  = Enforced   -- ^ value is read-only
  | Unenforced -- ^ value is read-write

-- | Synonym for a wrapped type with an enforced predicate.
type Strong p a = WithRefine 'Enforced   p a

-- | Synonym for a wrapped type with an unenforced predicate.
type Weak   p a = WithRefine 'Unenforced p a

-- | Enforce a wrapped value's predicate at runtime.
enforce
    :: forall p a. Predicate p a
    => WithRefine 'Unenforced p a
    -> Either RefineException (WithRefine 'Enforced p a)
enforce wrua =
    maybe (Right (WithRefine a)) Left (validate (Proxy @p) a)
  where a = unWithRefine wrua

-- | Enforce a wrapped value's predicate at runtime, calling 'fail' if the value
--   does not satisfy the predicate.
enforceFail
    :: (Predicate p a, MonadFail m)
    => WithRefine 'Unenforced p a
    -> m (WithRefine 'Enforced p a)
enforceFail wrua = case enforce wrua of
                    Left  e    -> fail $ displayException e
                    Right wrea -> pure wrea

-- | Stop enforcing a wrapped value's predicate.
unenforce :: WithRefine 'Enforced p a -> WithRefine 'Unenforced p a
unenforce = withRefine . unWithRefine

-- | If you have a @t'WithRefine' ''Enforced'@ value, you can obtain a matching
--   'Refined'.
enforcedToRefined :: WithRefine 'Enforced p a -> Refined p a
enforcedToRefined = reallyUnsafeRefine . unWithRefine

-- | If you have a 'Refined' value, you can obtain a matching @t'WithRefine'
--   ''Enforced'@.
refinedToEnforced :: Refined p a -> WithRefine 'Enforced p a
refinedToEnforced = reallyUnsafeEnforce . unrefine

-- | Construct an enforced type, completely ignoring any refinements!
--
-- You should only use this if you can prove that the refinement holds. (We need
-- it internally to coerce between 'Refined'.)
reallyUnsafeEnforce :: a -> WithRefine 'Enforced p a
reallyUnsafeEnforce = WithRefine

-- | Not very useful, but clarifies the meaning of enforced and unenforced
--   refinements.
type WithRefineRep :: forall p a r. PredicateStatus -> p -> a -> r
type family WithRefineRep ps p a where
    WithRefineRep 'Enforced   p a = Refined p a
    WithRefineRep 'Unenforced _ a = a
