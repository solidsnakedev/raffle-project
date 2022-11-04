module Main where

import Prelude
import qualified Utils
import qualified Lottery
import qualified MintTicket
import qualified MintRaffle
import System.Environment (getArgs)
import System.Directory (
    removeDirectoryRecursive ,
    createDirectoryIfMissing ,
    doesDirectoryExist
    )
import qualified Data.ByteString.Char8 as BS
import qualified Plutus.V1.Ledger.Value as Value

main = do
    -- [utxo, raffleName] <- getArgs
    -- let utxo' = Utils.parseUTxO utxo
    --     raffleName' = Value.tokenName $ BS.pack raffleName
    -- print utxo'
    -- print raffleName'
    exist <- doesDirectoryExist "plutus-scripts"
    createDirectoryIfMissing exist "plutus-script"
    -- Temporary disable MintRaffle, because a Native Script is used in Lucid
    -- Utils.writeValidator "plutus-scripts/mint-raffle.plutus" $ MintRaffle.getValidator $ MintRaffle.getScript $ MintRaffle.policy utxo' raffleName'
    Utils.writeValidator "plutus-scripts/lottery.plutus" $ Lottery.getValidator (MintTicket.getCurrencySymbol MintTicket.policy)
    Utils.writeValidator "plutus-scripts/mint-ticket.plutus" $ MintTicket.getValidator (MintTicket.getScript MintTicket.policy)
