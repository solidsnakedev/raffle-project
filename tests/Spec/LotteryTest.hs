{-# LANGUAGE NumericUnderscores  #-}

module Spec.LotteryTest where

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
import Plutus.V1.Ledger.Interval (
    always ,
    to ,
    from
    )
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
import Plutus.Script.Utils.Scripts (datumHash)
import PlutusTx (ToData (..))
import Plutus.V1.Ledger.Api (
    Datum(..)
    )
import PlutusTx.Builtins (
    appendByteString ,
    consByteString ,
    sha2_256 ,
    toBuiltin ,
    lengthOfByteString ,
    lessThanByteString
    )
import PlutusTx.Builtins.Class (
    stringToBuiltinByteString
    )

import qualified Data.ByteString.Char8 as BS
------------------
-- Project dependencies
------------------
import qualified Lottery
import Lottery (
    LotteryParam(..) ,
    LotteryDatum(..) ,
    LotteryRedeemer(..) ,
    MinHash (..)
    )
import qualified MintTicket

datum1 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 0
          , lotteryMinimumHash =  ""
          , lotteryTickets     = []
          , lotteryIntervals   = (90, 100)
          }

datum2 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 1
          , lotteryMinimumHash =  ""
          , lotteryTickets     = [(TokenName "ticket-1")]
          , lotteryIntervals   = (90, 100)    
          }

datum3 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 2
          , lotteryMinimumHash =  ""
          , lotteryTickets     = [ (TokenName "ticket-1")
                                 , (TokenName "ticket-2")
                                 ]
          , lotteryIntervals   = (90, 100)
          }

datum4 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 3
          , lotteryMinimumHash =  ""
          , lotteryTickets     = [ (TokenName "ticket-1")
                                 , (TokenName "ticket-2")
                                 , (TokenName "ticket-3")
                                 ]
          , lotteryIntervals   = (90, 100)
          }

-- lotteryMinimumHash calculated as follows
-- sha2_256 (appendByteString ticketName $ consByteString soldTickets raffleSeed)
datum5 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 3
          , lotteryMinimumHash = sha2_256 (appendByteString "ticket-1" $ consByteString 3 "adadadsds")
          , lotteryTickets     = [ (TokenName "ticket-1")
                                 , (TokenName "ticket-2")
                                 , (TokenName "ticket-3")
                                 ]
          , lotteryIntervals   = (90, 100)
          }

datum6 = LotteryDatum 
          { lotteryTicketPrice = 10_000_000
          , lotteryRandomSeed  = "adadadsds"
          , lotteryMaxTicket   = 3
          , lotterySoldTicket  = 3
          , lotteryMinimumHash = sha2_256 (appendByteString "ticket-2" $ consByteString 3 "adadadsds")
          , lotteryTickets     = [ (TokenName "ticket-1")
                                 , (TokenName "ticket-2")
                                 , (TokenName "ticket-3")
                                 ]
          , lotteryIntervals   = (90, 100)
          }

raffleNFT = singleton (CurrencySymbol "raflle-currency-symbol") (TokenName "raffle") 1

lotteryScriptAddress = Lottery.getScriptAddress (CurrencySymbol "ticket-currency-symbol")

tests = testGroup "- Lottery Test Cases"
           [ testCase "1. Testing Lottery Buy Redeemer" $ assertBool "Passed Validation" test1
           , testCase "2. Testing Lottery Buy Redeemer" $ assertBool "Passed Validation" test2
           , testCase "3. Testing Lottery Buy Redeemer" $ assertBool "Passed Validation" test3
           , testCase "4. Testing Lottery Claim Redeemer" $ assertBool "Passed Validation" test4
           , testCase "5. Testing Lottery Claim Redeemer" $ assertBool "Passed Validation" test5
           , testCase "6. Testing Lottery Close Redeemer" $ assertBool "Passed Validation" test6
           ]
    where
          test1 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum1 Buy context1
          test2 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum2 Buy context2
          test3 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum3 Buy context3
          test4 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum4 (Claim (TokenName "ticket-1")) context4
          test5 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum5 (Claim (TokenName "ticket-2")) context5
          test6 = Lottery.mkLotteryValidator (CurrencySymbol "ticket-currency-symbol") datum6 (Close (TokenName "ticket-2")) context6

-------------------------
-- Buy Ticket 1
-------------------------
context1 :: ScriptContext
context1 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-1"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 100_000_000
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-1"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 1_000_000
                                  <> raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum1
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-1") 1
                    , txOutDatumHash = Nothing
                    }
        txInfoOutputScript = 
            TxOut   { txOutAddress = lotteryScriptAddress
                    , txOutValue = singleton adaSymbol adaToken 1_000_000 <>
                                   singleton adaSymbol adaToken 10_000_000 <>
                                   raffleNFT
                    , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum2
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet, txInfoOutputScript]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-1") 1
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = to 50 -- Buy less than 90
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = [(datumHash $ Datum $ toBuiltinData datum2, Datum $ toBuiltinData datum2)]
                               , txInfoId            = TxId "script-tx-id-1"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-1"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }

-------------------------
-- Buy Ticket 2
-------------------------
context2 :: ScriptContext
context2 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-2"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 100_000_000
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-2"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 1_000_000
                                  <> raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum2
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                    , txOutDatumHash = Nothing
                    }
        txInfoOutputScript = 
            TxOut   { txOutAddress = lotteryScriptAddress
                    , txOutValue = singleton adaSymbol adaToken 1_000_000 <>
                                   singleton adaSymbol adaToken 10_000_000 <>
                                   raffleNFT
                    , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum3
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet, txInfoOutputScript]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = to 80 -- Buy less than 90
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = [(datumHash $ Datum $ toBuiltinData datum3, Datum $ toBuiltinData datum3)]
                               , txInfoId            = TxId "script-tx-id-2"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-2"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }

-------------------------
-- Buy Ticket 3
-------------------------
context3 :: ScriptContext
context3 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-3"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 100_000_000
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-3"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 1_000_000
                                  <> raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum3
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-3") 1
                    , txOutDatumHash = Nothing
                    }
        txInfoOutputScript = 
            TxOut   { txOutAddress = lotteryScriptAddress
                    , txOutValue = singleton adaSymbol adaToken 1_000_000 <>
                                   singleton adaSymbol adaToken 10_000_000 <>
                                   raffleNFT
                    , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum4
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet, txInfoOutputScript]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-3") 1
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = to 90 -- Buy less than 90
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = [(datumHash $ Datum $ toBuiltinData datum4, Datum $ toBuiltinData datum4)]
                               , txInfoId            = TxId "script-tx-id-3"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-3"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }


-------------------------
-- Try to Claim minHash with Ticket 1
-------------------------
context4 :: ScriptContext
context4 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-4"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                    singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-1") 1
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-4"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 20_000_000 <>
                                     raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum4
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-1") 1
                    , txOutDatumHash = Nothing
                    }
        txInfoOutputScript = 
            TxOut   { txOutAddress = lotteryScriptAddress
                    , txOutValue = singleton adaSymbol adaToken 20_000_000 <>
                                   raffleNFT
                    , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum5
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet, txInfoOutputScript]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = mempty
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = to 90 -- Claim intervals between 90 - 100
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = [(datumHash $ Datum $ toBuiltinData datum5, Datum $ toBuiltinData datum5)]
                               , txInfoId            = TxId "script-tx-id-4"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-4"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }

-------------------------
-- Try to Claim minHash with Ticket 2 (Ticket 2 is minHash due to -> sha2_256 (appendByteString "ticket-2" $ consByteString 3 "adadadsds"))
-------------------------
context5 :: ScriptContext
context5 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-5"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                    singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-5"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 20_000_000 <>
                                     raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum5
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                    , txOutDatumHash = Nothing
                    }
        txInfoOutputScript = 
            TxOut   { txOutAddress = lotteryScriptAddress
                    , txOutValue = singleton adaSymbol adaToken 20_000_000 <>
                                   raffleNFT
                    , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum6
                    }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet, txInfoOutputScript]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = mempty
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = to 95 -- Claim intervals between 90 - 100
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = [(datumHash $ Datum $ toBuiltinData datum6, Datum $ toBuiltinData datum6)]
                               , txInfoId            = TxId "script-tx-id-5"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-5"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }


context6 :: ScriptContext
context6 = 
    let txInfoInputWallet = TxInInfo
            TxOutRef { txOutRefId = TxId "wallet-tx-id-6"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                     , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                    singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                     , txOutDatumHash = Nothing
                     }
        txInfoInputScript = TxInInfo
            TxOutRef { txOutRefId = TxId "script-tx-id-6"
                     , txOutRefIdx =  0
                     }
            TxOut    { txOutAddress = lotteryScriptAddress
                     , txOutValue =  singleton adaSymbol adaToken 20_000_000 <>
                                     raffleNFT
                     , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum6
                     }
        txInfoOutputWallet = 
            TxOut   { txOutAddress = Address { addressCredential = PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", addressStakingCredential = Nothing }
                    , txOutValue = singleton adaSymbol adaToken 90_000_000 <>
                                   singleton (CurrencySymbol "ticket-currency-symbol") (TokenName "ticket-2") 1
                    , txOutDatumHash = Nothing
                    }
        --txInfoOutputScript = 
        --    TxOut   { txOutAddress = lotteryScriptAddress
        --            , txOutValue = singleton adaSymbol adaToken 20_000_000 <>
        --                           raffleNFT
        --            , txOutDatumHash = Just $ datumHash $ Datum $ toBuiltinData datum7
        --            }
    in 
    ScriptContext { scriptContextTxInfo = 
                        TxInfo { txInfoInputs        = [txInfoInputWallet, txInfoInputScript]
                               , txInfoOutputs       = [txInfoOutputWallet]
                               , txInfoFee           = singleton adaSymbol adaToken 1_000_000
                               , txInfoMint          = mempty
                               , txInfoDCert         = []
                               , txInfoWdrl          = []
                               , txInfoValidRange    = from 110 -- Close greater than 100
                               , txInfoSignatories   = [PubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"]
                               , txInfoData          = []
                               , txInfoId            = TxId "script-tx-id-6"
                               }
                  , scriptContextPurpose = Spending ( TxOutRef { txOutRefId = TxId "script-tx-id-6"
                                                               , txOutRefIdx =  0
                                                               }
                                                    )
                  }
-- find wallet PubKeyHash
-- filter (P.isJust ) $ pubKeyOutput <$> txInInfoResolved <$> (txInfoInputs $ scriptContextTxInfo context4)

-- find Value paid to PubKeyHash
-- pubKeyOutputsAt "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" $ scriptContextTxInfo context4

--testing = 
--    where
--        values = flattenValue $ valueProduced  (scriptContextTxInfo context4) of
--        token = filter ((cs, tn, amnt) -> cs == CurrencySymbol "ticket-currency-symbol" ) values


--t = toBuiltinData $ sha2_256 ""

--isMinHash =
--    case lengthOfByteString "ll" of
--            0 -> True
--            _ -> lessThanByteString "asa" "ll"
