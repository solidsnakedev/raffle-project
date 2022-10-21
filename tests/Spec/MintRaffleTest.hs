{-# LANGUAGE NumericUnderscores  #-}

module Spec.MintRaffleTest where

import Prelude
import Plutus.V1.Ledger.Contexts (
    ScriptContext (..),
    ScriptPurpose (..),
    TxInfo (..),
    TxInInfo (..),
    TxOutRef (..) ,
    TxId (..),
    TxOut (..)
    )
import Plutus.V1.Ledger.Value (
    CurrencySymbol (..) ,
    TokenName (..) ,
    singleton ,
    adaToken ,
    tokenName ,
    adaSymbol
    )
import Plutus.V1.Ledger.Address (
    Address (..)
    )
import Plutus.V1.Ledger.Time (
    POSIXTime (..)
    )
import Plutus.V1.Ledger.Crypto (
    PubKeyHash (..)
    )
import Plutus.V1.Ledger.Credential (
    Credential (..)
    )
import Plutus.V1.Ledger.Interval (always)
import Wallet.Emulator.Wallet

import Hedgehog (
    Property ,
    Group (..) ,
    property ,
    checkParallel ,
    (===) ,
    forAll
    )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (
    testGroup ,
    defaultMain
    )
import Test.Tasty.HUnit (
    testCase ,
    assertBool
    )
import Test.Tasty.ExpectedFailure (
    expectFailBecause
    )
import Test.Tasty.Hedgehog (
    testProperty
    )

import qualified MintRaffle

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 1000) Gen.alpha
    reverse (reverse xs) === xs

main :: IO Bool
main =
  checkParallel $ Group "Test.Example" [
      ("prop_reverse", prop_reverse)
    ]


tests = testGroup "- Raffle Test Cases" 
            [ testCase "1. Testing Raffle Minting Policy" $ assertBool "Passed Validation" test1
            , expectFailBecause "Fail because -> txOutRefIdx = 1" $ testCase "2. Testing Raffle Minting Policy" $ assertBool "Passed Validation" test2 
            , expectFailBecause "Fail because -> TokenName \"test1\"" $ testCase "3. Testing Raffle Minting Policy" $ assertBool "Passed Validation" test3
            , testProperty "4. prop_reverese" prop_reverse ]
    where
        test1 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  0}) (TokenName "test") () mintRaffleContext
        test2 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  1}) (TokenName "test") () mintRaffleContext
        test3 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  0}) (TokenName "test1") () mintRaffleContext
    


testMintRaffle ::  IO ()
testMintRaffle = do
    defaultMain $
        testGroup "Raffle Test Cases" 
            [ testCase "Testing Raffle Minting Policy" $ assertBool "Passed Validation" test1
            , expectFailBecause "Fail because -> txOutRefIdx = 1" $ testCase "Testing Raffle Minting Policy" $ assertBool "Passed Validation" test2 
            , expectFailBecause "Fail because -> TokenName \"test1\"" $ testCase "Testing Raffle Minting Policy" $ assertBool "Passed Validation" test3
            , testProperty "prop_reverese" prop_reverse ]
    where
        test1 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  0}) (TokenName "test") () mintRaffleContext
        test2 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  1}) (TokenName "test") () mintRaffleContext
        test3 = MintRaffle.mkPolicy (TxOutRef { txOutRefId = TxId "asdasd", txOutRefIdx =  0}) (TokenName "test1") () mintRaffleContext

mintRaffleContext :: ScriptContext
mintRaffleContext = 
    let txInfoInputs' = TxInInfo
            TxOutRef { txOutRefId = TxId "asdasd"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton "" "" 100_000_000
                     , txOutDatumHash = Nothing
                     }
        txInfoOutputs' = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 100_000_000
                    , txOutDatumHash = Nothing
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputs']
                               , txInfoOutputs       = [txInfoOutputs']
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = singleton (CurrencySymbol "test") (TokenName "test") 1
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = always @POSIXTime
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = []
                               , txInfoId            = TxId "asdasd"
                               }
                  , scriptContextPurpose = Minting (CurrencySymbol "test")
                  }