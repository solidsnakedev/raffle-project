module MintTicket where

import           PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Plutus.V1.Ledger.Scripts        as Plutus.V1
import qualified Plutus.Script.Utils.V1.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import           Ledger.Value                as Value
import           Ledger.Constraints     as Constraints

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo


-- Remove this --> TODO: make sure to add the CurrencySymbol of the Raffle as parameter so the raffle and the tickets can be related
-- TODO: eliminate CurrencySymbol, it's not necessary because if validation pass then you can add the TokenName to the list and make sure
-- only those tokens can claim
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy utxo ctx = case mintedValue of
    [(_cs, tn, n)] -> validateMint tn n
    _              -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash =
      sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    checkForOverflow :: Bool
    checkForOverflow = txOutRefIdx utxo < 256

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount =
      hasUTxO utxo ctx &&
      amount == 1 &&
      validateTokenName tn &&
      checkForOverflow ||
      amount == (-1)

policy :: PSU.V1.MintingPolicy
policy = Plutus.V1.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy mkPolicy||])


getScript :: PSU.V1.MintingPolicy -> Plutus.V1.Script
getScript = Plutus.V1.unMintingPolicyScript

getValidator :: Plutus.V1.Script -> Plutus.V1.Validator
getValidator = Plutus.V1.Validator

getCurrencySymbol :: PSU.V1.MintingPolicy -> CurrencySymbol
getCurrencySymbol = PSU.V1.scriptCurrencySymbol

