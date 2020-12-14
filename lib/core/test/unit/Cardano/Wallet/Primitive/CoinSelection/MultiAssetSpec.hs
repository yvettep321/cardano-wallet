{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.MultiAssetSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.MultiAsset
    ( padCoalesce )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( Sum (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.QuickCheck
    ( Arbitrary (..), Property, genericShrink, property, (===) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = describe "Multi-Asset Coin Selection" $

    modifyMaxSuccess (const 1000) $ do

        parallel $ describe "padCoalesce" $ do
            it "prop_padCoalesce_length" $
                property $ prop_padCoalesce_length @(Sum Int)
            it "prop_padCoalesce_sort" $
                property $ prop_padCoalesce_sort @(Sum Int)
            it "prop_padCoalesce_sum" $
                property $ prop_padCoalesce_sum @(Sum Int)

prop_padCoalesce_length
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_length source target =
    NE.length (padCoalesce source target) === NE.length target

prop_padCoalesce_sort
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sort source target =
    NE.sort result === result
  where
    result = padCoalesce source target

prop_padCoalesce_sum
    :: (Monoid a, Ord a, Show a) => NonEmpty a -> NonEmpty () -> Property
prop_padCoalesce_sum source target =
    F.fold source === F.fold (padCoalesce source target)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink
