module Utils where

import Prelude as P
import qualified Plutus.V1.Ledger.Scripts as LedgerScripts
import qualified Plutus.Script.Utils.V1.Scripts as UtilsScripts
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short  as SBS
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Cardano.Api as CardanoAPI hiding (TxId)
import           PlutusTx              
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import Ledger.Tx.CardanoAPI (toCardanoScriptData, toCardanoScriptHash)
import Data.Text 
import GHC.Word (Word32)
import Ledger (TxOutRef(..), TxId(..))
import Ledger.Bytes                        (getLedgerBytes)
import Data.String                         (IsString (..))

import PlutusTx.Sqrt

import PlutusTx.Prelude (indexByteString, BuiltinByteString, lengthOfByteString)


--dataToScriptData :: Data -> ScriptData
--dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
--dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
--dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
--dataToScriptData (I n)         = ScriptDataNumber n
--dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
--writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData
writeJSON file = LBS.writeFile file .
                 encode . 
                 scriptDataToJson ScriptDataJsonDetailedSchema .
                 toCardanoScriptData .
                 PlutusTx.toBuiltinData

writeUnit :: IO ()
writeUnit = do
    exist <- doesDirectoryExist "output"
    createDirectoryIfMissing exist "output"
    writeJSON "output/unit.json" ()
    putStrLn "unit value saved in -> output/unit.json"

writeValidator :: FilePath -> LedgerScripts.Validator -> IO (Either (FileError ()) ())
writeValidator file validator = do
    CardanoAPI.writeFileTextEnvelope file Nothing scriptSerialised
    where
        script :: LedgerScripts.Script
        script = LedgerScripts.unValidatorScript validator

        scriptShortBS :: SBS.ShortByteString
        scriptShortBS = SBS.toShort . LBS.toStrict $ serialise script
        
        scriptSerialised :: PlutusScript PlutusScriptV1
        scriptSerialised = PlutusScriptSerialised scriptShortBS

testnet_id :: CardanoAPI.NetworkId
testnet_id = Testnet $ NetworkMagic (fromInteger 1)

toTestnetId :: Word32 -> CardanoAPI.NetworkId
toTestnetId n = CardanoAPI.fromNetworkMagic (NetworkMagic n)

mkShelleyAddr :: CardanoAPI.PaymentCredential -> CardanoAPI.StakeAddressReference -> CardanoAPI.Address ShelleyAddr
mkShelleyAddr paymentCredential stakeAddressReference = makeShelleyAddress testnet_id paymentCredential stakeAddressReference

toScriptHash :: LedgerScripts.Validator -> CardanoAPI.ScriptHash
toScriptHash validator = 
    case toCardanoScriptHash validatorHash of
        Left e -> error (show e)
        Right scriptHash -> scriptHash
    where
        validatorHash = UtilsScripts.validatorHash validator

toBech32Addr :: LedgerScripts.Validator -> CardanoAPI.NetworkId -> Text
toBech32Addr validator testnetId = serialiseToBech32 shelleyAddress
    where
        shelleyAddress = makeShelleyAddress testnetId paymentCredentialByScript CardanoAPI.NoStakeAddress
        paymentCredentialByScript = CardanoAPI.PaymentCredentialByScript $ toScriptHash validator

toPlutusCore = LedgerScripts.unScript . LedgerScripts.unValidatorScript


parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = P.span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ P.read $ P.tail y

-- prettyPrintJSON $ sha2_256 "hello, world!"

--PlutusTx.Prelude.map (indexByteString (sha2_256 "hello")) [0 .. 31]

--Data.ByteString.unpack $ (fromBuiltin $ sha2_256 "hello")

--sum $ PlutusTx.Prelude.map (indexByteString ("e1c0c4132b498ae79762c7df068834b7e2b4841e6ce934084a1ecf94dccbff97")) [0 .. 63]

--sum $ PlutusTx.Prelude.map (indexByteString ("e2c0c4132b498ae79762c7df068834b7e2b4841e6ce934084a1ecf94dccbff96")) [0 .. 63]

-- v = product $ PlutusTx.Prelude.map (indexByteString ("e1c0c4142b498ae79762c7df068834b7e2b4841e6ce934184a2ecf94dccbff97")) [0 .. 63]
-- bigNum = 200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

-- utxo in this case works as a nonce
-- sum $ PlutusTx.Prelude.map (indexByteString (sha2_256 ("e1c0c3132b498ae79662c8df068814b7e2b4841e6ce924084b1ecf94dccfff94"))) [0 .. 31]

fromApprox :: Sqrt -> Integer
fromApprox (Approximately i ) = i
fromApprox (Exactly i) = i
fromApprox _ = 0

isqrtInt :: Integer -> Integer
isqrtInt i = fromApprox $ isqrt i

-- Dummy data to play in terminal
-- http://localhost:8002/haddock/plutus-example/html/PlutusExample-ScriptContextChecker.html#v:sampleTestV1ScriptContextDataJSON


go :: Integer -> BuiltinByteString -> Integer
go i b
    | i == lengthOfByteString b = 0
    | otherwise = indexByteString b i + (go (i + 1) b)

-- |Helper function to convert bytestrings to integers
--byteString2Integer :: BS.ByteString -> Integer
--byteString2Integer = BS.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0