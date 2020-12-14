{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.CoinSelection.MultiAsset
    where

import Prelude

import Data.Function.Utils
    ( applyN )
import Data.List.NonEmpty
    ( NonEmpty (..) )

import qualified Data.List.NonEmpty as NE

-- | Adjusts the source list so that its length is the same as the target list,
--   either by padding the list, or by coalescing a subset of the elements.
--
-- If the source list is shorter than the target list, this function repeatedly
-- inserts 'mempty' into the list until the desired length has been reached.
--
-- If the source list is longer than the target list, this function repeatedly
-- coalesces the smallest pair of elements with '<>' until the desired length
-- has been reached.
--
-- The resulting list is guaranteed to be sorted into ascending order, and the
-- sum of the elements is guaranteed to be the same as the sum of elements in
-- the source list.
--
-- Examples (shown with ordinary list notation):
--
-- >>> padCoalesce [Sum 1] [(), (), (), ()]
-- [Sum 0, Sum 0, Sum 0, Sum 1]
--
-- >>> padCoalesce [Sum (-1)] [(), (), (), ()]
-- [Sum (-1), Sum 0, Sum 0, Sum 0]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] [(), (), ()]
-- [Sum 3, Sum 4, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] [(), ()]
-- [Sum 7, Sum 8]
--
-- >>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] [()]
-- [Sum 15]
--
padCoalesce :: forall m. (Monoid m, Ord m)
    => NonEmpty m
    -- ^ Source list
    -> NonEmpty ()
    -- ^ Target list
    -> NonEmpty m
padCoalesce sourceUnsorted target
    | sourceLength < targetLength =
        applyN (targetLength - sourceLength) pad source
    | sourceLength > targetLength =
        applyN (sourceLength - targetLength) coalesce source
    | otherwise =
        source
  where
    source = NE.sort sourceUnsorted

    sourceLength = NE.length source
    targetLength = NE.length target

    pad :: NonEmpty m -> NonEmpty m
    pad = NE.insert mempty

    coalesce :: NonEmpty m -> NonEmpty m
    coalesce (x :| y : zs) = NE.insert (x <> y) zs
    coalesce xs = xs
