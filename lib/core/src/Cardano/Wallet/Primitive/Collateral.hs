{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- For a UTxO to be considered a suitable collateral input, it must:
--    - Be a pure ADA UTxO (no tokens)
--    - Require a verification key witness to be spent
--    - Have an output address that is not any of:
--      - a native script address
--      - a plutus script address
--
-- UTxOs of this kind are sometimes referred to as "VK" inputs.

module Cardano.Wallet.Primitive.Collateral
    ( classifyCollateralAddress
    , asCollateral
    , AddrNotSuitableForCollateral(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..) )

import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Crypto as L
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle

-- | If the given @(TxIn, TxOut)@ represents a UTxO that is suitable for use as
-- a collateral input, returns @Just@ along with the total ADA value of the
-- UTxO. Otherwise returns @Nothing@ if it is not a suitable collateral value.
asCollateral
    :: (TxIn, TxOut)
    -- ^ TxIn, TxOut representing a UTxO
    -> Maybe Coin
    -- ^ The total ADA value of that UTxO if it is suitable for collateral,
    -- otherwise Nothing.
asCollateral (_txIn, txOut) = do
   coin <- TokenBundle.toCoin $ tokens txOut

   case classifyCollateralAddress (address txOut) of
     Left IsAScriptAddr ->
         Nothing
     Left IsAStakeAddr ->
         Nothing
     Left IsAMalformedOrUnknownAddr ->
         Nothing
     Right _addr ->
         Just coin

-- | Reasons why an address might be considered unsuitable for a collateral
-- input.
data AddrNotSuitableForCollateral
    = IsAScriptAddr
    -- ^ The address is some form of script address
    | IsAStakeAddr
    -- ^ The address is some form of stake address
    | IsAMalformedOrUnknownAddr
    -- ^ The address could not be parsed
    deriving (Eq, Show)

-- | Analyze an address to determine if it's funds are suitable for use as a
-- collateral input.
--
-- This function returns an Either instead of a Maybe because I think it's
-- important these functions return why they failed. It is extremely useful for
-- debugging programs.
classifyCollateralAddress
    :: Address
    -> Either AddrNotSuitableForCollateral Address
classifyCollateralAddress addr@(Address addrBytes) =
    case L.deserialiseAddr addrBytes of
        -- If we couldn't deserialise the address, it's either a stake address,
        -- a malformed address, or an address the Ledger doesn't know about.
        Nothing ->
            -- Test if it's a stake address (a.k.a. reward account address)
            case L.deserialiseRewardAcnt addrBytes of
                Nothing ->
                    Left IsAMalformedOrUnknownAddr
                Just (_ :: L.RewardAcnt L.StandardCrypto) ->
                    Left IsAStakeAddr

        -- This is a bootstrap address, therefore a suitable collateral input.
        Just (L.AddrBootstrap _bootstrapAddr) ->
            Right addr

        -- Otherwise, we further analyze the address.
        Just (L.Addr _network payCred _stakeRef) ->
            case (payCred :: L.Credential 'L.Payment L.StandardCrypto) of
                -- Check if this is a script address.
                L.ScriptHashObj _scriptHash ->
                    -- This is a native script address or a Plutus script
                    -- address, therefore not a suitable collateral input.
                    Left IsAScriptAddr

                -- Otherwise, this is an address that corresponds with a
                -- suitable collateral input
                L.KeyHashObj (_keyHash :: L.KeyHash 'L.Payment L.StandardCrypto)
                    -> Right addr
