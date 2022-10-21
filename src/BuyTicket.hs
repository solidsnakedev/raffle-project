module BuyTicket where

import           PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
--import qualified Plutus.V2.Ledger.Scripts        as Plutus.V2 -- this does not exists
import qualified Plutus.V1.Ledger.Api                 as Plutus.V1
import qualified Plutus.Script.Utils.V1.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUT.V1
import           Ledger.Value                as Value

import           Prelude (IO, Show, String, Semigroup (..), show, undefined, fromIntegral)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Text.Printf            (printf)
import qualified Data.ByteString.Lazy  as LBS
import           Wallet.Emulator.Wallet
import           Plutus.Trace
import           Ledger.Constraints     as Constraints
import           Data.Void (Void)
import           Control.Monad (void)


{-# INLINABLE hasUTxO #-}
hasUTxO :: Plutus.V1.TxOutRef -> Plutus.V1.ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> Plutus.V1.txInInfoOutRef i == utxo) $ Plutus.V1.txInfoInputs (info ctx)

{-# INLINABLE info #-}
info :: Plutus.V1.ScriptContext -> Plutus.V1.TxInfo
info = Plutus.V1.scriptContextTxInfo

{-# INLINEABLE mintFlattened #-}
mintFlattened :: Plutus.V1.ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ Plutus.V1.txInfoMint (info ctx)

--------------------------
-- Validator Parameters
--------------------------

data TicketParam = TicketParam
    { ticketCurrSymbol    :: CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''TicketParam

--------------------------
-- Datum typed
--  TokenName (is the hashed utxo of the minted ticket)
--  BuiltinByteString (is a hashed secret number, to be reveal at the end of the game)
--------------------------
-- https://github.com/input-output-hk/plutus/issues/4193
-- Do not use bang patterns like ![(TokenName,BuiltinByteString)]
data TicketDatum = TicketDatum 
                 { players :: [(TokenName,BuiltinByteString)]
                 , ticketPrice :: Integer
                 }
    deriving (Show, Eq)

--instance Eq TicketDatum where
--    {-# INLINABLE (==) #-}
--    TicketDatum [] == TicketDatum [] = True
--    TicketDatum (x:xs) == TicketDatum (y:ys) = x == y && xs == ys
--    _ == _ = False 

PlutusTx.unstableMakeIsData ''TicketDatum


{-# INLINABLE mkTicketVal #-}
mkTicketVal :: TicketParam -> TicketDatum -> () -> Plutus.V1.ScriptContext -> Bool
mkTicketVal ticketParam ticketDatum _ ctx = True
    where 
    validateMint :: Bool
    validateMint = case mintFlattened ctx of
      [(cs, tn, amt)] -> True
        --                 (cs == lenderNftCs) &&
        --                 (tn == lenderTn) &&
        --                 (amt == 1)
      _               -> False

data Typed
instance PSUT.V1.ValidatorTypes Typed where
  type DatumType Typed = TicketDatum

ticketTypedVal :: TicketParam -> PSUT.V1.TypedValidator Typed
ticketTypedVal = PSUT.V1.mkTypedValidatorParam @Typed
    $$(PlutusTx.compile [|| mkTicketVal ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSUT.V1.mkUntypedValidator

ticketValidator :: TicketParam -> PSU.V1.Validator
ticketValidator = PSUT.V1.validatorScript . ticketTypedVal

ticketValHash :: TicketParam -> PSU.V1.ValidatorHash
ticketValHash = PSUT.V1.validatorHash . ticketTypedVal

ticketScrAddress :: TicketParam -> Ledger.Address
ticketScrAddress = PSUT.V1.validatorAddress . ticketTypedVal
