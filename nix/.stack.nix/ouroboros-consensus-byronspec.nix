{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ouroboros-consensus-byronspec";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "ByronSpec ledger integration in the Ouroboros consensus layer";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "877ce057ff6fb086474c8eaad53f2b7f0e0fce6b";
      sha256 = "1kp0qysfy3hl96a3a61rijascq36f1imh3z4jy0vyiygb6qrv47z";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "877ce057ff6fb086474c8eaad53f2b7f0e0fce6b";
      sha256 = "1kp0qysfy3hl96a3a61rijascq36f1imh3z4jy0vyiygb6qrv47z";
      };
    postUnpack = "sourceRoot+=/ouroboros-consensus-byronspec; echo source root reset to \$sourceRoot";
    }