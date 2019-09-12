{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Data.Algebra.Ranges
Description : Algebra of sets
Copyright   : (c) Isaac van Bakel, 2019
License     : GPL-3
Maintainer  : ivb@vanbakel.io
Stability   : experimental

The algebra on ranges. This provides an 'Algebra' instance for any 'Foldable',
'MonadPlus' collection of 'R.Interval's - so it should in particular work with
lists and sets, which are the expected ways to use this algbera.

The internal calculations are written to not perform any prettifying steps, so
several successive operations may produce highly ugly and unusual intervals
such as @[3, -7)@ - if you want a representation of only non-empty, non-redundant
intervals, use 'simplify'.
|-}
module Data.Algebra.Ranges
  ( {- instance -}
    simplify
  , Data.Algebra.Ranges.union
  , invert
  , contains
  ) where

import qualified Data.Ranges as R
import           Data.Algebra
  ( Algebra (..)
  )

import           Control.Monad
  ( MonadPlus (..)
  , mfilter
  , msum
  )

-- | The algebra on intervals.
instance (Foldable t, MonadPlus t, Ord a) => Algebra (t (R.Interval a)) where
  unit = pure R.Everything

  containedIn left right
    = all (\interval -> any (interval `R.containedIn`) (simplify right)) left

  intersection left right
    = cross (\l r -> pure (R.intersection l r)) left right

    where
      cross :: (MonadPlus m) => (a -> a -> m b) -> m a -> m a -> m b
      cross f left right = do
        l <- left
        r <- right
        f l r

  bar val
    = foldr intersection unit (invert <$> val)

-- | Take the union of two intervals, whether they are contiguous or not. Unlike
-- 'R.contiguousUnion', this is a total function.
union :: (MonadPlus t, Ord a) => R.Interval a -> R.Interval a -> t (R.Interval a)
union left right
  = if R.contiguous left right
      then pure (R.contiguousUnion left right)
      else pure left `mplus` pure right

-- | Invert an interval, producing zero or more intervals.
invert :: (MonadPlus t) => R.Interval a -> t (R.Interval a)
invert R.Everything = mzero
invert (R.Closed from to) = pure (R.ToOpen from) `mplus` pure (R.FromOpen to)
invert (R.Open from to) = pure (R.ToClosed from) `mplus` pure (R.FromClosed to)
invert (R.ClosedOpen from to) = pure (R.ToOpen from) `mplus` pure (R.FromClosed to)
invert (R.OpenClosed from to) = pure (R.ToClosed from) `mplus` pure (R.FromOpen to)
invert (R.ToOpen to) = pure (R.FromClosed to)
invert (R.ToClosed to) = pure (R.FromOpen to)
invert (R.FromOpen from) = pure (R.ToClosed from)
invert (R.FromClosed from) = pure (R.ToOpen from)

-- | Simplify an element in the algebra of 'R.Interval's by eliminating
-- any empty intervals, and collapsing redundant intervals together
simplify :: (Foldable t, MonadPlus t, Ord a) => t (R.Interval a) -> t (R.Interval a)
simplify =
  -- Combine ranges which are contiguous
  foldr
    (\interval simplified ->
        let disjoint = mfilter (not . R.contiguous interval) simplified
            joinable = mfilter (R.contiguous interval) simplified
        in pure (foldr R.contiguousUnion interval joinable) `mplus` disjoint
    )
    mzero
  -- Eliminate empty ranges
  . mfilter (not . R.isEmpty)

-- | Does the element of the algebra of 'R.Interval's contain the given element?
contains :: (Ord a, Foldable t) => a -> t (R.Interval a) -> Bool
contains a values = any (R.contains a) values
