module MintRaffle where

import qualified PlutusTx
--import           Prelude                 (Semigroup (..), Show (..))
import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified Plutus.Script.Utils.V1.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUT.V1
import qualified Ledger.Value                as Value
import           Plutus.V1.Ledger.Api   as Plutus.V1

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo

    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

policy :: TxOutRef -> TokenName -> MintingPolicy
policy oref tn = Plutus.V1.mkMintingPolicyScript $
    --Converts a custom redeemer from a minting policy function to an untyped minting policy function
    $$(PlutusTx.compile [|| \oref' tn' -> PSUT.V1.mkUntypedMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

getScript :: MintingPolicy -> Script
getScript = Plutus.V1.unMintingPolicyScript

getValidator :: Script -> Validator
getValidator = Validator

getCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
getCurrencySymbol oref tn = PSU.V1.scriptCurrencySymbol $ policy oref tn
