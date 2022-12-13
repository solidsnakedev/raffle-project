module Lottery where

import           PlutusTx
import           PlutusTx.Builtins (lessThanByteString)
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
--import           Ledger                      hiding (mint, singleton)
--import qualified Plutus.V1.Ledger.Scripts        as Plutus.V1 -- this does not exists
--import qualified Plutus.V1.Ledger.Api                 as Plutus.V1
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Api (
    Datum(..) ,
    POSIXTime
    )
import Plutus.V1.Ledger.Interval (
    contains ,
    from ,
    to ,
    after ,
    overlaps ,
    interval
    )
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

-- TODO: add [TokenName] as list so you can add only the tokens minted and validated,
-- also create dummy raffle NFT and dummy validator and ticket minting policy, so it's easy to test contracts

data LotteryDatum = LotteryDatum 
    { ticketPrice :: Integer
    , randomSeed  :: BuiltinByteString
    , maxTickets   :: Integer
    , soldTickets  :: Integer
    , minimumHash :: BuiltinByteString
    , ticketList :: [TokenName]
    , intervals :: (POSIXTime, POSIXTime)
    } deriving Show

-- lotteryBuyDeadLine :: POSIXTime
-- lotteryClaimInterval

-- Buy logic -- Deadline
-- contains (to 90) (to 80) -- arguments #1 deadline #2 slotnumber -> PASS
-- contains (to 90) (to 90) -- arguments #1 deadline #2 slotnumber -> PASS
-- contains (to 90) (to 91) -- arguments #1 deadline #2 slotnumber -> FAIL

-- Claim logic - In between
-- overlaps (interval 90 100) (to 89) && --arguments #1 deadline #2 starttime #3 slotnumber -> PASS
-- Plutus.V1.Ledger.Interval.after 100 (to 89)  -- arguments #1 startime #2 slotnumber
-- (\ x y z -> overlaps (interval x y) (to z) && Plutus.V1.Ledger.Interval.after y (to z)) 90 100 102

-- Close logic -- StartTime
-- contains (from 100) (from 50) -- arguments #1 startime #2 slotnumber -> FAIL
-- contains (from 100) (from 100) -- arguments #1 startime #2 slotnumber -> PASS
-- contains (from 100) (from 101) -- arguments #1 startime #2 slotnumber -> PASS



instance Eq LotteryDatum where
    {-# INLINABLE (==) #-}
    LotteryDatum tp rs mt st mh tns intv == LotteryDatum tp' rs' mt' st' mh' tns' intv' =
        (tp == tp') &&
        (rs == rs') &&
        (mt == mt') &&
        (st == st') &&
        (mh == mh') &&
        (tns == tns') &&
        (intv == intv)


PlutusTx.unstableMakeIsData ''LotteryDatum

data LotteryRedeemer = Buy | Claim TokenName | Close TokenName

PlutusTx.unstableMakeIsData ''LotteryRedeemer

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE checkDatumBuy #-}
checkDatumBuy :: LotteryDatum -> LotteryDatum -> Bool
checkDatumBuy old new = 
    ticketPrice old     == ticketPrice new &&
    randomSeed  old     == randomSeed  new &&
    maxTickets  old     == maxTickets  new &&
    soldTickets old + 1 == soldTickets new &&
    minimumHash old     == minimumHash new &&
    intervals   old     == intervals   new

-- Checks everything except the minimumHash
{-# INLINABLE checkDatumClaim #-}
checkDatumClaim :: LotteryDatum -> LotteryDatum -> Bool
checkDatumClaim old new =
    ticketPrice old     == ticketPrice new &&
    randomSeed  old     == randomSeed  new &&
    maxTickets  old     == maxTickets  new &&
    soldTickets old     == soldTickets new &&
    intervals   old     == intervals   new


-- TODO: Consider convert code to Plutarch
{-# INLINABLE mkLotteryValidator #-}
mkLotteryValidator :: CurrencySymbol -> LotteryDatum -> LotteryRedeemer -> ScriptContext -> Bool
mkLotteryValidator csMintTicket dat red ctx =
    case red of
        Buy ->
            checkDatumBuy dat outputDatum &&
            traceIfFalse "Lottery List is empty" isLotteryListEmpty &&
            traceIfFalse "Token name is not in Lottery List" isTokenInLotteryList &&
            traceIfFalse "Previous Tokens not included in Lottery List" hasPreviousTokens &&
            traceIfFalse "Need to pay Ticket Price to Contract" isPayToScript &&
            traceIfFalse "Can not Buy more tickets" (soldTickets outputDatum <= maxTickets dat) -- &&
            --traceIfFalse "Buy dead line reached" isBuyPeriod

        Claim tokenNameRedeemer ->
            checkDatumClaim dat outputDatum  &&
            traceIfFalse "Token was not purchased in Buy State" (isTokenPurchased tokenNameRedeemer) &&
            traceIfFalse "Invalid Minimum Hash" (validateMinHash tokenNameRedeemer)&&
            traceIfFalse "Result is not minimum than previous Hash" isMinHash &&
            traceIfFalse "Funds in Script can not be spend until Close action" isFundInScript &&
            traceIfFalse "Can not Claim until all tickets are sold" (soldTickets dat == maxTickets dat) -- &&
            --traceIfFalse "Not Claim period" isClaimPeriod

        Close tokenNameRedeemer ->
            traceIfFalse "Can not Close until all tickets are sold" (soldTickets dat == maxTickets dat) && -- Add expiration day instead
            traceIfFalse "Invalid Minimum Hash" (validateCloseMinHash tokenNameRedeemer) -- &&
            --traceIfFalse "Not Close period" isClosePeriod

    where

        txInfo' :: TxInfo
        txInfo' = info ctx

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "Missing inputs"
            Just txInInfo' -> txInInfoResolved txInInfo'

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "Expected exactly one Lottery output"

        getMaybeDatum :: Maybe LotteryDatum
        getMaybeDatum = do
            dh        <- txOutDatumHash ownOutput
            (Datum d) <- findDatum dh txInfo'
            fromBuiltinData d

        outputDatum :: LotteryDatum
        outputDatum =
            case getMaybeDatum of
                Nothing -> traceError "Lottery output datum not found"
                Just d -> d

        isLotteryListEmpty :: Bool
        isLotteryListEmpty = not $ null $ ticketList outputDatum

        isTokenInLotteryList :: Bool
        isTokenInLotteryList = case flattenValue $ txInfoMint txInfo' of
            [( _ , tokenName', _ )] -> any ((==) tokenName') $ ticketList outputDatum
            _ -> False

        hasPreviousTokens :: Bool
        hasPreviousTokens = 
            let len = length $ ticketList outputDatum
            in 
            ticketList dat == take (len - 1) (ticketList outputDatum)

        isPayToScript :: Bool
        isPayToScript =
            adaOutput `geq` (adaInput <> adaTicketPrice) && -- check that ticket price in ADA is added to the script output
            raffleNFTInput == raffleNFTOutput && -- check if raffle NFT is added to the script output
            valueLength == 2 -- Make sure that only ADA Value and Raffle NFT is added to the script output
            
            where
                adaOutput = adaOnlyValue $ txOutValue ownOutput
                adaInput = adaOnlyValue $ txOutValue ownInput
                adaTicketPrice = singleton adaSymbol adaToken (ticketPrice outputDatum)

                raffleNFTInput = noAdaValue $ txOutValue ownInput
                raffleNFTOutput = noAdaValue $ txOutValue ownOutput

                valueLength = length $ symbols $ txOutValue ownOutput

        isTokenPurchased :: TokenName -> Bool
        isTokenPurchased tokenNameRedeemer = (findSpentTicket tokenNameRedeemer) `elem` ticketList outputDatum
        
        findSpentTicket :: TokenName -> TokenName
        findSpentTicket tokenNameRedeemer =
            case result of
                Nothing -> traceError "Could not find ticket in wallet"
                Just (_,ticket,_) -> ticket
            where
                result = find (\(cs, tn, amnt) -> cs == csMintTicket && amnt == 1 && tn == tokenNameRedeemer) values
                values = flattenValue $ valueSpent txInfo'

        validateMinHash :: TokenName -> Bool
        validateMinHash tokenNameRedeemer =
            (expectedHash == minimumHash') &&
            minimumHash' /= mempty

            where
                minimumHash' = minimumHash outputDatum
                TokenName bsSpentTicket = findSpentTicket tokenNameRedeemer
                expectedHash = sha2_256 (appendByteString bsSpentTicket $ consByteString (soldTickets outputDatum) (randomSeed outputDatum))

        isMinHash :: Bool
        isMinHash 
            | (minimumHash dat) == mempty = True
            | otherwise = lessThanByteString (minimumHash outputDatum) (minimumHash dat)
       
        -- Check if the Script input and output values are the same
        isFundInScript :: Bool
        isFundInScript =
            valueOutput == valueInput
            where
                valueOutput = txOutValue ownOutput
                valueInput  = txOutValue ownInput

        validateCloseMinHash :: TokenName -> Bool
        validateCloseMinHash tokenNameRedeemer =
            (expectedHash == minimumHash') &&
            minimumHash' /= mempty
            where
                minimumHash' = minimumHash dat
                TokenName bsSpentTicket = (findSpentTicket tokenNameRedeemer)
                expectedHash = sha2_256 (appendByteString bsSpentTicket $ consByteString (soldTickets dat) (randomSeed dat))

        -- Contract Period
        -- ---------->|------------>|------------->
        -- buy-period  claim-period  close-period

        isBuyPeriod :: Bool
        isBuyPeriod =
            contains (to endPeriod) range
            where
                endPeriod = fst $ intervals outputDatum
                range = txInfoValidRange txInfo'

        isClaimPeriod :: Bool
        isClaimPeriod =
            overlaps (interval startPeriod endPeriod) range &&
            after endPeriod range
            where
                startPeriod = fst $ intervals outputDatum -- these are constants
                endPeriod   = snd $ intervals outputDatum
                range = txInfoValidRange txInfo'

        isClosePeriod :: Bool
        isClosePeriod =
            contains (from startPeriod) range
            where
                startPeriod = snd $ intervals dat -- these are constants
                range = txInfoValidRange txInfo'

data Typed
instance PSUT.V1.ValidatorTypes Typed where
  type DatumType Typed = LotteryDatum
  type RedeemerType Typed = LotteryRedeemer

typedValidator :: CurrencySymbol -> PSUT.V1.TypedValidator Typed
typedValidator = PSUT.V1.mkTypedValidatorParam @Typed
    $$(PlutusTx.compile [|| mkLotteryValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSUT.V1.mkUntypedValidator

getValidator :: CurrencySymbol -> PSU.V1.Validator
getValidator = PSUT.V1.validatorScript . typedValidator

getValidatorHash :: CurrencySymbol -> PSU.V1.ValidatorHash
getValidatorHash = PSUT.V1.validatorHash . typedValidator

getScriptAddress :: CurrencySymbol -> Address
getScriptAddress = PSUT.V1.validatorAddress . typedValidator
