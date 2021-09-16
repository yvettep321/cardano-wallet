{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Gen
    ( genBlockHeader, genSlotNo )
import Cardano.Wallet.Network
    ( ErrPostTx (..), FollowLog (..), emptyStats, updateStats )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..) )
import Data.Time.Clock
    ( getCurrentTime )
import NoThunks.Class
    ( wNoThunks )
import Test.Hspec
    ( Spec, describe, expectationFailure, it )
import Test.QuickCheck
    ( Arbitrary (..), getNonEmpty, oneof, property )

import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrPostTxValidationError mempty

    describe "updateStats" $ do
        it "results in no unexpected thunks" $ property $ \(msg :: FollowLog ()) -> do
            -- This test is not /fully/ fool-proof. Adding lots of nested types to
            -- LogState and logic in updateStats not covered by the generator
            -- might cause us to miss a thunk.
            --
            -- But it does provide some sanity.
            t <- getCurrentTime
            let s0 = emptyStats t
            let s = updateStats msg s0
            wNoThunks [] s >>= \case
                Nothing -> return ()
                Just x -> expectationFailure $ show x

instance Arbitrary (FollowLog msg) where
    arbitrary = oneof
      [ MsgApplyBlocks
          <$> arbitrary
          <*> ((NE.fromList . getNonEmpty) <$> arbitrary)
      , MsgDidRollback
          <$> genSlotNo
          <*> genSlotNo
      , MsgFollowerTip . Just
          <$> arbitrary
      , pure $ MsgFollowerTip Nothing
      , pure MsgHaltMonitoring
      ]
  -- Shrinking not that important here

instance Arbitrary BlockHeader where
    arbitrary = genBlockHeader =<< genSlotNo

testShow :: Show a => a -> Spec
testShow a = it (show a) True
