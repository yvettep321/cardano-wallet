{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.PreludeSpec (spec) where

import Cardano.Wallet.Prelude

import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, property, (===) )

import qualified Data.Text as T

spec :: Spec
spec = describe "showText" $ do
    it "works" $ property (prop_showTextRead @Int)

prop_showTextRead :: (Eq a, Read a, Show a) => a -> Property
prop_showTextRead a = read (T.unpack (showText a)) === a
