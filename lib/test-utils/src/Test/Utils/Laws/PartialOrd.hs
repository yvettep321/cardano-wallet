{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils.Laws.PartialOrd
    ( partialOrdLaws
    ) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..), Property, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

partialOrdLaws
    :: forall a. (PartialOrd a, Arbitrary a, Show a) => Proxy a -> Laws
partialOrdLaws p = Laws "PartialOrd"
    [ ("Reflexivity"
        , partialOrdReflexive p)
    , ("Antisymmetry"
        , partialOrdAntisymmetric p)
    , ("Transitivity"
        , partialOrdTransitive p)
    ]

partialOrdReflexive
    :: forall a. (PartialOrd a, Arbitrary a, Show a) => Proxy a -> Property
partialOrdReflexive _ =
    property $ \(a :: a) -> a `leq` a

partialOrdAntisymmetric
    :: forall a. (PartialOrd a, Arbitrary a, Show a) => Proxy a -> Property
partialOrdAntisymmetric _ =
    property $ \(a :: a) b -> ((a `leq` b) && (b `leq` a)) == (a == b)

partialOrdTransitive
    :: forall a. (PartialOrd a, Arbitrary a, Show a) => Proxy a -> Property
partialOrdTransitive _ = property test
  where
    test (a :: a) b c
        | a `leq` b && b `leq` c = a `leq` c
        | a `leq` c && c `leq` b = a `leq` b
        | b `leq` a && a `leq` c = b `leq` c
        | b `leq` c && c `leq` a = b `leq` a
        | c `leq` a && a `leq` b = c `leq` b
        | c `leq` b && b `leq` a = c `leq` a
        | otherwise = True
