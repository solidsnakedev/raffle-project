{-# LANGUAGE NumericUnderscores  #-}

module BuyTicketEmulator where

------------------
-- Non Plutus dependencies
------------------
import           Prelude                (Semigroup (..), Show (..), IO, String)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Control.Monad (void)
import qualified Data.Map               as Map
import           Text.Printf            (printf)
import qualified Control.Monad.Freer.Extras as Extras

------------------
-- Plutus dependencies
------------------
import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Value   (TokenName (..), AssetClass(..))
import qualified Plutus.V1.Ledger.Value           as Value
import           Plutus.V1.Ledger.Api (
  TokenName (..),
  CurrencySymbol(..),
  Value(..),
  Address(..),
  TxOutRef (..),
  TxId (..),
  Redeemer(..),
  Datum(..)
  )
import           Ledger (ChainIndexTxOut (..), minAdaTxOut)
import           Plutus.Contract        (Contract, Endpoint)
import           Plutus.Contract        as Contract
import qualified Plutus.Trace           as Trace
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Tx
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.Ada          as Ada

------------------
-- Project dependencies
------------------
import Lottery (LotteryDatum(..), LotteryRedeemer(..))
import qualified Lottery
import qualified MintTicket
--import BuyTicket (TicketParam(..), TicketDatum(..))
--import qualified BuyTicket
import qualified MintRaffle

data RaffleParams = RaffleParams
    { rpWalletTokenName     :: !TokenName
    , rpWalletAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

data StartParams = StartParams
    { spTicketPrice     :: !Integer
    , spRandomSeed      :: !BuiltinByteString
    , spMaxTickets      :: !Integer
    , spSoldTickets     :: !Integer
    , spMinimumHash     :: !BuiltinByteString
    , spRaffleTokenName :: !TokenName
    , spWalletAddress   :: !Address
    } deriving (Generic, ToJSON, FromJSON, Show)

data BuyParams = BuyParams
    { bpTicketPrice     :: !Integer
    , bpRandomSeed      :: !BuiltinByteString
    , bpMaxTickets      :: !Integer
    , bpSoldTickets     :: !Integer
    , bpMinimumHash     :: !BuiltinByteString
    , bpRaffleTokenName :: !TokenName
    , bpWalletAddress   :: !Address
    } deriving (Generic, ToJSON, FromJSON, Show)

type RaffleSchema = Endpoint "mintRaffle" RaffleParams
                 .\/ Endpoint "startRaffle" StartParams
                 .\/ Endpoint "buyTicket" BuyParams
                 .\/ Endpoint "debugBuyTicket" BuyParams

mintRaffle :: RaffleParams -> Contract String RaffleSchema Text ()
mintRaffle params = do
    utxos <- Contract.utxosAt $ rpWalletAddress params
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = rpWalletTokenName params
                raffleCurSymbol = MintRaffle.getCurrencySymbol oref tn
                val     = Value.singleton raffleCurSymbol tn (1)
                lookups = Constraints.plutusV1MintingPolicy (MintRaffle.policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ Ledger.Tx.getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

startRaffle :: StartParams -> Contract String RaffleSchema Text ()
startRaffle params = do
    utxos <- ownUtxos
    (raffleCs, raffleTn, raffleAmnt, _, _) <- findRaffleNFT (spWalletAddress params) (spRaffleTokenName params)
    let ticketCurSymbol = MintTicket.getCurrencySymbol MintTicket.policy
        lotteryValidatorHash = Lottery.getValidatorHash ticketCurSymbol
        d = LotteryDatum
                { ticketPrice = spTicketPrice params
                , randomSeed  = spRandomSeed  params
                , maxTickets   = spMaxTickets  params
                , soldTickets  = spSoldTickets params
                , minimumHash = spMinimumHash params
                }
        v = Ada.toValue minAdaTxOut <> Value.singleton raffleCs raffleTn raffleAmnt
        tx = Constraints.mustPayToOtherScript lotteryValidatorHash (Datum $ PlutusTx.toBuiltinData d) v
    ledgerTx <- Contract.submitTx tx
    Contract.logInfo @String $ printf "MintTicket policy created: %s" (show ticketCurSymbol)
    Contract.logInfo @String $ printf "Lottery Script created: %s" (show lotteryValidatorHash)
    Contract.logInfo @String $ printf "Sending Raffle NFT: %s" (show $ (raffleCs, raffleTn, raffleAmnt))
    Contract.logInfo @String $ printf "Sending Raffle Datum: %s" (show $ PlutusTx.toBuiltinData d)
    void $ Contract.awaitTxConfirmed $ Ledger.Tx.getCardanoTxId ledgerTx
    

buyTicket :: BuyParams -> Contract String RaffleSchema Text ()
buyTicket params = do
    let ticketCurSymbol = MintTicket.getCurrencySymbol MintTicket.policy
        lotteryValidatorHash = Lottery.getValidatorHash ticketCurSymbol
    (raffleCs, raffleTn, raffleAmnt, lotteryTxOutRef, lotteryChainIndexTxOut) <- findRaffleNFT (Lottery.getScriptAddress ticketCurSymbol) (bpRaffleTokenName params)
    Contract.logInfo @String $ printf "Raffle NFT found in utxo lottery script | TxOutRef : %s" (show lotteryTxOutRef)
    Contract.logInfo @String $ printf "Raffle NFT found in utxo lottery script | ChainIndexTxOut : %s" (show lotteryChainIndexTxOut)
    utxosLotteryScript <- Contract.utxosAt $ Lottery.getScriptAddress ticketCurSymbol
    utxos <- Contract.utxosAt $ bpWalletAddress params
    lotteryDatum <- findLotteryDatum lotteryChainIndexTxOut
    let lotteryDatum' = lotteryDatum  {randomSeed = "test"}
    let oref = head $ Map.keys utxos
    let utxoTokenName       = consByteString (txOutRefIdx oref) (getTxId $ txOutRefId oref)
        hashedUtxoTokenName = sha2_256 utxoTokenName
        val                 = Value.singleton (ticketCurSymbol) (TokenName hashedUtxoTokenName) 1
        v = Ada.toValue minAdaTxOut <> Value.singleton raffleCs raffleTn raffleAmnt
        lookups             = Constraints.plutusV1MintingPolicy (MintTicket.policy)
                            <> Constraints.unspentOutputs utxos
                            <> Constraints.plutusV1OtherScript (Lottery.getValidator ticketCurSymbol)
                            <> Constraints.unspentOutputs utxosLotteryScript
        tx                  = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData oref) val
                            <> Constraints.mustSpendScriptOutput lotteryTxOutRef (Redeemer $ PlutusTx.toBuiltinData Buy)
                            <> Constraints.mustPayToOtherScript lotteryValidatorHash (Datum $ PlutusTx.toBuiltinData (lotteryDatum')) v
    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    Contract.logInfo @String $ printf "Ticket minted : %s" (show val)
    void $ Contract.awaitTxConfirmed $ Ledger.Tx.getCardanoTxId ledgerTx

debugBuyTicket :: BuyParams -> Contract String RaffleSchema Text ()
debugBuyTicket params = do
    let ticketCurSymbol = MintTicket.getCurrencySymbol MintTicket.policy
    (_, _, _, o, t) <- findRaffleNFT (Lottery.getScriptAddress ticketCurSymbol) (bpRaffleTokenName params)
    Contract.logInfo @String $ printf "lotteryChainIndexTxOut %s" (show (t,o) )


endpoints :: Contract String RaffleSchema Text ()
endpoints = do 
    awaitPromise (mintRaffle' `select` startRaffle' `select` buyTicket' `select` debugBuyTicket')
    endpoints
  where
    mintRaffle' = Contract.endpoint @"mintRaffle" mintRaffle
    startRaffle' = Contract.endpoint @"startRaffle" startRaffle
    buyTicket' = Contract.endpoint @"buyTicket" buyTicket
    debugBuyTicket' = Contract.endpoint @"debugBuyTicket" debugBuyTicket

test1 :: IO ()
test1 = Trace.runEmulatorTraceIO $ do
    let w1 = Wallet.knownWallet 1
    h1 <- Trace.activateContractWallet w1 endpoints
    Trace.callEndpoint @"mintRaffle" h1 $ RaffleParams "Raffle" (Wallet.mockWalletAddress w1)


test2 :: IO ()
test2 = Trace.runEmulatorTraceIO $ do
    let w1 = Wallet.knownWallet 1
    let w2 = Wallet.knownWallet 2
    h1 <- Trace.activateContractWallet w1 endpoints
    h2 <- Trace.activateContractWallet w2 endpoints

    Trace.callEndpoint @"mintRaffle" h1 $ RaffleParams { rpWalletAddress = (Wallet.mockWalletAddress w1)
                                                        ,rpWalletTokenName = "Raffle"
                                                        }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"startRaffle" h1 $ StartParams { spTicketPrice = 10_000_000
                                                       , spRandomSeed  = "3342542"
                                                       , spMaxTickets  = 3
                                                       , spSoldTickets = 0
                                                       , spMinimumHash =  ""
                                                       , spWalletAddress  = (Wallet.mockWalletAddress w1)
                                                       , spRaffleTokenName = "Raffle"
                                                       }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"buyTicket" h1 $ BuyParams { bpTicketPrice = 10_000_000
                                                    , bpRandomSeed  = "3342542"
                                                    , bpMaxTickets  = 3
                                                    , bpSoldTickets = 0
                                                    , bpMinimumHash =  ""
                                                    , bpWalletAddress  = (Wallet.mockWalletAddress w1)
                                                    , bpRaffleTokenName = "Raffle"
                                                    }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"debugBuyTicket" h1 $ BuyParams { bpTicketPrice = 10_000_000
                                                        , bpRandomSeed  = "3342542"
                                                        , bpMaxTickets  = 3
                                                        , bpSoldTickets = 0
                                                        , bpMinimumHash =  ""
                                                        , bpWalletAddress  = (Wallet.mockWalletAddress w1)
                                                        , bpRaffleTokenName = "Raffle"
                                                        }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"buyTicket" h2 $ BuyParams { bpTicketPrice = 10_000_000
                                                    , bpRandomSeed  = "3342542"
                                                    , bpMaxTickets  = 3
                                                    , bpSoldTickets = 0
                                                    , bpMinimumHash =  ""
                                                    , bpWalletAddress  = (Wallet.mockWalletAddress w2)
                                                    , bpRaffleTokenName = "Raffle"
                                                    }
    void $ Trace.waitUntilSlot 2
    Trace.callEndpoint @"debugBuyTicket" h2 $ BuyParams { bpTicketPrice = 10_000_000
                                                        , bpRandomSeed  = "3342542"
                                                        , bpMaxTickets  = 3
                                                        , bpSoldTickets = 0
                                                        , bpMinimumHash =  ""
                                                        , bpWalletAddress  = (Wallet.mockWalletAddress w2)
                                                        , bpRaffleTokenName = "Raffle"
                                                        }
test3 :: IO ()
test3 = Trace.runEmulatorTraceIO test3Trace

test3Trace :: Trace.EmulatorTrace ()
test3Trace = do
    let w1 = Wallet.knownWallet 1
        p = BuyParams   { bpTicketPrice = 10_000_000
                        , bpRandomSeed  = "3342542"
                        , bpMaxTickets  = 3
                        , bpSoldTickets = 0
                        , bpMinimumHash =  ""
                        , bpWalletAddress     = (Wallet.mockWalletAddress w1)
                        , bpRaffleTokenName = "Raffle"
                        }
    void $ Trace.activateContractWallet w1 (buyTicket p)
    void $ Trace.waitNSlots 2


findRaffleNFT :: Address -> TokenName -> Contract w s Text (CurrencySymbol, TokenName, Integer, TxOutRef, ChainIndexTxOut)
findRaffleNFT address tokenName = do
    mapUtxos <- utxosAt $ address
    let utxos = Map.toList mapUtxos
    case find f utxos of
        Nothing -> Contract.throwError "No NFT found on the provied Address!"
        Just (oref, chainIndex) -> do
            let values = Value.flattenValue (_ciTxOutValue chainIndex)
                filteredValues = filter (\(_, tn, _) -> tn == tokenName) values
            case filteredValues of
                [(cs,tn, amount)] -> return (cs,tn, amount, oref, chainIndex)
                _ -> Contract.throwError "Expected exactly one Value!"
    where
        f :: (TxOutRef, ChainIndexTxOut) -> Bool
        f (_, o) = any (\(_, tn, _) -> tn == tokenName) values
            where
                values = Value.flattenValue  $ _ciTxOutValue o

findLotteryDatum :: ChainIndexTxOut -> Contract w s Text LotteryDatum
findLotteryDatum chainIndex = do
    case snd $ _ciTxOutScriptDatum chainIndex of
        Nothing -> Contract.throwError "No Datum found on the provided Address!"
        Just (Datum d) -> 
            case PlutusTx.fromBuiltinData d of
                Nothing -> Contract.throwError "No instance FromData!"
                Just lotteryDatum -> return lotteryDatum
    --maybeDatum <- Contract.datumFromHash $ fst $ _ciTxOutScriptDatum chainIndex
    --case maybeDatum of
    --    Nothing -> Contract.throwError "No Datum found on the provided Address!"
    --    Just (Datum d) -> 
    --        case PlutusTx.fromBuiltinData d of
    --            Nothing -> Contract.throwError "No instance FromData!"
    --            Just lotteryDatum -> return lotteryDatum