module MintRaffleEmulator where

------------------
-- Plutus dependencies
------------------
import           PlutusTx.Prelude hiding (Semigroup (..))
import           Plutus.V1.Ledger.Api   (TokenName, Address)
import           Plutus.Contract        (Contract, Endpoint)
import qualified Plutus.Contract        as Contract
import qualified Plutus.Trace           as Trace
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Value           as Value
import           Ledger.Tx (getCardanoTxId)
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

------------------
-- Project dependencies
------------------
import qualified MintRaffle


data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- Contract.utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (MintRaffle.getCurrencySymbol oref tn) tn (1)
                lookups = Constraints.plutusV1MintingPolicy (MintRaffle.policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = Contract.awaitPromise $ Contract.endpoint @"mint" mint

testMintRaffle1 :: IO ()
testMintRaffle1 = Trace.runEmulatorTraceIO $ do
    let tn = "Raffle"
        w1 = Wallet.knownWallet 1
        w2 = Wallet.knownWallet 2
    h1 <- Trace.activateContractWallet w1 endpoints
    h2 <- Trace.activateContractWallet w2 endpoints
    Trace.callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = Wallet.mockWalletAddress w1
        }
    Trace.callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = Wallet.mockWalletAddress w2
        }
    void $ Trace.waitNSlots 1
