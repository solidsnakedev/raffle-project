module MintTicketEmulator where

------------------
-- Plutus dependencies
------------------
import           PlutusTx.Prelude hiding (Semigroup (..))
import           Plutus.V1.Ledger.Api (
  TokenName (..),
  Address(..),
  CurrencySymbol(..),
  Redeemer(..),
  TxOutRef (..),
  TxId (..)
  )
import           Ledger (ChainIndexTxOut (..))
import qualified PlutusTx
import           Plutus.Contract        as Contract
import qualified Plutus.Trace           as Trace
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Value           as Value
import           Ledger.Tx              (getCardanoTxId)
import qualified Wallet.Emulator.Wallet as Wallet

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
--import qualified Data.ByteString.Lazy  as LBS

------------------
-- Project dependencies
------------------
import qualified MintTicket

-- Dummy token policy of a raffle to link the minting of the tickets
dummyRaffleCurSymbol = CurrencySymbol "raffle-policy-id"

data NFTParams = NFTParams
    { walletAddress :: !Address
    , tokenPolicy :: !CurrencySymbol
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams
             .\/ Endpoint "burn" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint nftParams = do
    utxos <- Contract.utxosAt $ walletAddress nftParams
    case Map.keys utxos of
      [] -> Contract.logError @String "no utxo found"
      oref:_  -> do
        let utxoTokenName       = consByteString (txOutRefIdx oref) (getTxId $ txOutRefId oref)
            hashedUtxoTokenName = sha2_256 utxoTokenName
            val                 = Value.singleton (tokenPolicy nftParams) (TokenName hashedUtxoTokenName) 1
            lookups             = Constraints.plutusV1MintingPolicy MintTicket.policy <> Constraints.unspentOutputs utxos
            tx                  = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData oref) val
        ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
        void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx

        Contract.logInfo @String $ printf "forged %s" (show val)

burn :: NFTParams -> Contract w NFTSchema Text ()
burn nftParams = do
    utxos <- Contract.utxosAt $ walletAddress nftParams
    (oref, o, tn) <- findNFT nftParams
    let val = Value.singleton (tokenPolicy nftParams) (tn) (-1)
        lookups = Constraints.plutusV1MintingPolicy MintTicket.policy <> Constraints.unspentOutputs utxos
        tx = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData oref) val
    Contract.logInfo @String $ printf "burn log %s" (show val)
    ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
    void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx


findNFT :: NFTParams -> Contract w s Text (TxOutRef, ChainIndexTxOut, TokenName)
findNFT nftParams = do
  utxos <- utxosAt $ walletAddress nftParams
  let xs =  Map.toList utxos
  case find f xs of
    Nothing -> Contract.throwError "No NFT found on the provied Address!"
    Just (oref, o) -> do
      let values = Value.flattenValue (_ciTxOutValue o)
      let filteredValues = filter (\(curSymbol, _, _) -> curSymbol == tokenPolicy nftParams) values
      case filteredValues of
        [(_,tn, _)] -> return (oref, o, tn)
        _ -> Contract.throwError "Expected exactly one Value!"
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = any ((==) (tokenPolicy nftParams)) sym
      where
        sym = Value.symbols $ _ciTxOutValue o

endpoints :: Contract () NFTSchema Text ()
endpoints = do 
  awaitPromise (mint' `select` burn') 
  endpoints
  where
    mint' = endpoint @"mint" mint
    burn' = endpoint @"burn" burn

testMintTicket1 :: IO ()
testMintTicket1 = Trace.runEmulatorTraceIO $ do
    let w1 = Wallet.knownWallet 1
        tockenPolicy' = MintTicket.getCurrencySymbol MintTicket.policy
    h1 <- Trace.activateContractWallet w1 endpoints
    Trace.callEndpoint @"mint" h1 $ NFTParams {walletAddress = Wallet.mockWalletAddress w1, tokenPolicy = tockenPolicy'}
    void <- Trace.waitNSlots 2
    Trace.callEndpoint @"burn" h1 $ NFTParams {walletAddress = Wallet.mockWalletAddress w1, tokenPolicy = tockenPolicy'}

testMintTicket2 :: IO ()
testMintTicket2 = Trace.runEmulatorTraceIO $ do
    let w1 = Wallet.knownWallet 1
        w2 = Wallet.knownWallet 2
        w3 = Wallet.knownWallet 3
        tockenPolicy' = MintTicket.getCurrencySymbol MintTicket.policy
    h1 <- Trace.activateContractWallet w1 endpoints
    h2 <- Trace.activateContractWallet w2 endpoints
    h3 <- Trace.activateContractWallet w3 endpoints
    Trace.callEndpoint @"mint" h1 $ NFTParams {walletAddress = Wallet.mockWalletAddress w1, tokenPolicy = tockenPolicy'}
    void <- Trace.waitNSlots 2
    Trace.callEndpoint @"mint" h2 $ NFTParams {walletAddress = Wallet.mockWalletAddress w2, tokenPolicy = tockenPolicy'}
    void <- Trace.waitNSlots 2
    Trace.callEndpoint @"mint" h3 $ NFTParams {walletAddress = Wallet.mockWalletAddress w3, tokenPolicy = tockenPolicy'}