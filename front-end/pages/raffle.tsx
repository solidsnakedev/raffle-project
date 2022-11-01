import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import { Lucid, TxHash, Lovelace, Constr, SpendingValidator, Data, utf8ToHex, Script, MintingPolicy, Datum, datumJsonToCbor, toHex, sha256, concat, Unit, hexToUtf8 } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'
import {exportedForTesting as helios_internal} from '@hyperionbt/helios'
import { min_fee } from 'lucid-cardano/types/src/core/wasm_modules/cardano_multiplatform_lib_web/cardano_multiplatform_lib'
import { sign } from 'crypto'
import { setPriority } from 'os'
import { mintTicketValidator, lotteryValidator } from '../utils/validators'
import { Console } from 'console'


const Raffle: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")
  const [rafflePolicyId, setRafflePolicyId] = useState("")
  const [rafflePolicy, setRafflePolicy]= useState<Script>({type: "Native", script:""})
  const [ticketPrice, setTicketPrice] = useState(10000000)
  const [randomSeed, setRandomSeed ] = useState("adadadsds")
  const [maxTicket, setMaxTicket] = useState(3)
  const [minimumHash, setMinimumHash] = useState("")
  const [tickets, setTickets] = useState([])

  useEffect(() => {
    if (lucid) {
      /**
      * One shot function to Initialise raffle policy.
      */
      const initRafflePolicy = async () => {
        const {paymentCredential} = lucid.utils.getAddressDetails(await lucid.wallet.address());
        const mintingPolicy = lucid.utils.nativeScriptFromJson(
          {
              type: "sig",
              keyHash: paymentCredential?.hash
          }
        )
        console.log('minting policy : ', mintingPolicy)
        const policyId = lucid.utils.mintingPolicyToId(
            mintingPolicy,
        );
        setRafflePolicyId(policyId)
        setRafflePolicy(mintingPolicy)
      }
      initRafflePolicy()
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])
  
  /**
    * Convert seconds to miliseconds.
    */
  const toMiliseconds = (seconds : number) => {
    return Math.floor(seconds * 1000)
  }

  const getUtxos = async () => {
    if (lucid) {
      // const utxos = await lucid.utxosAt(scriptAddress);
      // if (utxos.length > 0) {
      //   utxos.map(async (value: any) => {
      //     console.log(value.txHash)
      //   });
      // } else {
      //   console.log("no utxos in script")
      // }


      console.log(helios.bytesToHex(helios_internal.Crypto.sha2_256(helios_internal.stringToBytes("asda"))))

      console.log(lucid.utils.unixTimeToSlot(Date.now() + 1000000))
      console.log(helios.deserializeUplc(`{"type":"PlutusScriptV1", "cborHex": "${lotteryValidator.script}"}`).toString())
      // amount.map(async (asset: any) => {
      //console.log(utxos);
    }
  }

  const mintRaffle = async () => {
    if (lucid) {
        console.log("minting raffle")
        console.log('raffle name : ', utf8ToHex("RaffleNFT #1"))

        const unit = rafflePolicyId + utf8ToHex("RaffleNFT #1");
        console.log('Raffle Unit : ', unit)
        
        const asset = {[unit]: BigInt(1)}
        console.log('Asset :', asset)

        const tx = await lucid.newTx().
                         mintAssets(asset).
                         validTo(Date.now()+toMiliseconds(100)).
                         attachMintingPolicy(rafflePolicy).
                         complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submited:', txHash)
    }
  }


  const burnRaffle = async () => {
    if (lucid) {
        console.log("burning raffle")

        const unit = rafflePolicyId + utf8ToHex("RaffleNFT #1");
        console.log('Raffle Unit : ', unit)
    
        const asset = {[unit]: BigInt(-1)}
        console.log('Asset :', asset)

        const tx = 
            await lucid.newTx().
            mintAssets(asset).
            validTo(Date.now()+ toMiliseconds(100)).
            attachMintingPolicy(rafflePolicy).
            complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submited:', txHash)
    }
  }

  const burnTickets = async () => {
    if (lucid) {
      console.log("burning tickets")

      const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
      const walletUtxos = await lucid.wallet.getUtxos()
      console.log(walletUtxos)
      const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                          .filter(asset => asset.includes(mintTicketPolicyId))
      console.log(ticketAssets)
      const assetsToBurn = ticketAssets.reduce((result, item) => {
        return { ...result, [item]: BigInt(-1)}
      },{})

      console.log(assetsToBurn)

      const assetsToBurnList = ticketAssets.map(asset => { return {[asset]: BigInt(1)}})
      console.log(assetsToBurnList)
      
      const walletTxHash = walletUtxos[0].txHash
      const walletOutputIndex = walletUtxos[0].outputIndex
      const txHashBytes = helios.hexToBytes(walletTxHash) // helios
      console.log('txHashBytes : ', txHashBytes)
      //const id = new Uint8Array([walletOutputIndex])  // lucid
      const id = walletOutputIndex
      //const tokenName = concat(id,txhash) // lucid
      const tokenName = [id].concat(txHashBytes) // concatenate uint8 numbers
      console.log('concat : ', tokenName)
      //const sha = sha256(tokenName) // sha256 from lucid
      const hashedTokenNameBytes = helios_internal.Crypto.sha2_256(tokenName) //sha256 from helios
      console.log('hashedTokenNameBytes : ', hashedTokenNameBytes)
      //console.log('hex : ', toHex(new Uint8Array(sha))) // Lucid
      const hashedTokenName = helios.bytesToHex(hashedTokenNameBytes)
      console.log('hashedTokenName : ', hashedTokenName)

      const walletTxHashData = new Constr (0, [walletUtxos[0].txHash]) // refers to TxId Constructor in PlutusTx
      const mintingRedeemerData = new Constr (0, [ walletTxHashData , BigInt(walletUtxos[0].outputIndex)]) // refers TxOutRef constructor in PlutusTx
      const mintingRedeemer = Data.to(mintingRedeemerData) // TxOutRef to CBOR

      const unitTicket = mintTicketPolicyId + hashedTokenName
      const assetTicket = {[unitTicket]: BigInt(1)}
      const tx = 
          await lucid.newTx().
          mintAssets(assetTicket, mintingRedeemer).
          attachMintingPolicy(mintTicketValidator).
          validTo(Date.now()+ toMiliseconds(100)).
          complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      console.log('Transaction submited:', txHash)
    }
  }

  const startRaffle = async () => {
    if (lucid) {
        console.log("starting raffle")
        console.log('raffle policy id : ', rafflePolicyId)
        const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
        console.log('lottery validator address : ', lotteryValidatorAdd)
        console.log('mint ticket validator address : ', lucid.utils.validatorToAddress(mintTicketValidator))
        const lotteryTicketPrice = 10000000;
        const lotteryRandomSeed = "adadadsds"
        const lotteryMaxTicket = 3;
        const lotterySoldTicket = 0
        const lotteryMinimumHash = "";
        const lotteryTickets :string[] = [];
        // [buy period] | [claim period] | [close period]
        // ------------ 2 -------------- 4 --------------
        const lotteryIntervals = new Constr (0,[
            BigInt(Date.now()+ toMiliseconds(600)), // 10 minutes
            BigInt(Date.now()+ toMiliseconds(1200)) //  20 minutes
        ])
        
        const lotteryDatum =
        new Constr (0, [
            BigInt(lotteryTicketPrice),
            utf8ToHex(lotteryRandomSeed),
            BigInt(lotteryMaxTicket),
            BigInt(lotterySoldTicket),
            utf8ToHex(lotteryMinimumHash),
            lotteryTickets,
            lotteryIntervals
        ])
        console.log('lottery datum (PlutusData) : ', lotteryDatum)

        const datum = Data.to(lotteryDatum) // serialise to CBOR
        console.log('lottery datum (CBOR) : ', datum)
        
        //console.log(lucid.utils.unixTimeToSlot(Date.now()))
        const unit : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
        const utxos = await lucid.utxosAtWithUnit(await lucid.wallet.address(), unit)

        if (utxos.length > 0) {
            const tx = await
                lucid.newTx().
                payToContract(lotteryValidatorAdd, datum, {[unit]: BigInt(1)}).
                complete()
        
            const signedTx = await
                tx.sign().
                complete()
        
            const txHash = signedTx.submit()
            console.log('Transaction submited:', txHash)

        } else {
            alert('Raffle not found in wallet address')
        }

    }
  }

  const buyTicket = async () => {
    if (lucid){
        const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
        console.log('lottery validator address : ', lotteryValidatorAdd)
        const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
        const assetRaffle = {[unitRaffle]: BigInt(1)}
        const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
        const walletUtxos = await lucid.wallet.getUtxos()

        if (walletUtxos.length && scriptUtxos.length > 0) {
            console.log('wallet txHash : ', walletUtxos[0].txHash)
            console.log('outputIndex : ', walletUtxos[0].outputIndex)
            const walletTxHash = walletUtxos[0].txHash
            const walletOutputIndex = walletUtxos[0].outputIndex

            const txHashBytes = helios.hexToBytes(walletTxHash) // helios
            console.log('txHashBytes : ', txHashBytes)
            //const id = new Uint8Array([walletOutputIndex])  // lucid
            const id = walletOutputIndex
            //const tokenName = concat(id,txhash) // lucid
            const tokenName = [id].concat(txHashBytes) // concatenate uint8 numbers
            console.log('concat : ', tokenName)
            //const sha = sha256(tokenName) // sha256 from lucid
            const hashedTokenNameBytes = helios_internal.Crypto.sha2_256(tokenName) //sha256 from helios
            console.log('hashedTokenNameBytes : ', hashedTokenNameBytes)
            //console.log('hex : ', toHex(new Uint8Array(sha))) // Lucid
            const hashedTokenName = helios.bytesToHex(hashedTokenNameBytes)
            console.log('hashedTokenName : ', hashedTokenName)

            console.log('utxo found in lottery validator : ', scriptUtxos)
            if (scriptUtxos[0].datumHash) {
                const utxoDatum = Data.from (await lucid.datumOf(scriptUtxos[0])) // from CBOR to PlutusData/Json
                console.log('script datum found(LotteryDatum) : ', utxoDatum)
                const newDatum = {
                    lotteryTicketPrice : utxoDatum.fields[0],
                    lotteryRandomSeed : utxoDatum.fields[1],
                    lotteryMaxTicket : utxoDatum.fields[2],
                    lotterySoldTicket : utxoDatum.fields[3],
                    lotteryMinimumHash : utxoDatum.fields[4],
                    lotteryTickets : utxoDatum.fields[5],
                    lotteryIntervals : utxoDatum.fields[6]
                }
                const newLotteryDatum = new
                    Constr (0, [
                        newDatum.lotteryTicketPrice,
                        newDatum.lotteryRandomSeed,
                        newDatum.lotteryMaxTicket,
                        newDatum.lotterySoldTicket + BigInt(1),
                        newDatum.lotteryMinimumHash,
                        newDatum.lotteryTickets.concat(hashedTokenName),
                        newDatum.lotteryIntervals
                    ])
                console.log('new script datum(LotteryDatum): ', newLotteryDatum)
                const datum = Data.to(newLotteryDatum)
                console.log('new script datum(CBOR):', datum)
                const validatorRedeemer = Data.to(new Constr (0,[])) // Buy redeemer
                console.log('validatorRedeemer(Buy) : ', validatorRedeemer)
                const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
                const unitTicket = mintTicketPolicyId + hashedTokenName
                const assetTicket = {[unitTicket]: BigInt(1)}
                console.log('assetTicket : ', assetTicket)
                const walletTxHashData = new Constr (0, [walletTxHash]) // refers to TxId Constructor in PlutusTx
                const mintingRedeemerData = new Constr (0, [ walletTxHashData , BigInt(walletOutputIndex)]) // refers TxOutRef constructor in PlutusTx
                const mintingRedeemer = Data.to(mintingRedeemerData) // TxOutRef to CBOR
                console.log('mintingRedeemerData(TxOutRef) : ', mintingRedeemerData)
                const lovelace : Lovelace = BigInt(32000000)
                //const lovelace  = ""
                console.log('mintingRedeemer(CBOR) : ', mintingRedeemer)
                try {
                  const tx = await lucid.newTx()
                             .collectFrom([scriptUtxos[0],walletUtxos[0]],validatorRedeemer)
                             .attachSpendingValidator(lotteryValidator)
                             .attachMintingPolicy(mintTicketValidator)
                             .mintAssets(assetTicket, mintingRedeemer)
                             .payToContract(lotteryValidatorAdd, datum,{lovelace, [unitRaffle]: BigInt(1)})
                             .validTo(Date.now() + toMiliseconds(100))
                             .complete()
                  console.log(await lucid.provider.getProtocolParameters())
                  //console.log(min_fee(tx,['minFeeA'],))
                  // lucid.txBuilderConfig.
                  const signedTx = await tx.sign().complete()
                  const txHash = signedTx.submit()
                  console.log('Transaction submited:', txHash)
                } catch (err) {
                  alert(err)
                }
                // Just minting ticket policy to test validation
                //const tx = await lucid.newTx()
                //          .collectFrom([walletUtxos[0]])
                //          .attachMintingPolicy(mintTicketValidator)
                //          .mintAssets(assetTicket, mintingRedeemer)
                //          .complete()
                //console.log(tx)
            }
        }
        else {
            alert('no utxos found in loterry validator address : ' + lotteryValidatorAdd)
        }
        
  }}

  const claim = async () => {
    if (lucid){
      console.log("Claiming")
      const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
      const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
      const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
      const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
      const walletUtxos = await lucid.wallet.getUtxos()
      console.log(walletUtxos)
      const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                          .filter(asset => asset.includes(mintTicketPolicyId))
      console.log('tickets found in wallet:', ticketAssets)

      
      if (walletUtxos.length && scriptUtxos.length > 0) {
        if (scriptUtxos[0].datumHash) {
          const utxoDatum = Data.from (await lucid.datumOf(scriptUtxos[0])) // from CBOR to PlutusData/Json
          console.log('script datum found(LotteryDatum) : ', utxoDatum)
          const newDatum = {
            lotteryTicketPrice : utxoDatum.fields[0],
            lotteryRandomSeed : utxoDatum.fields[1],
            lotteryMaxTicket : utxoDatum.fields[2],
            lotterySoldTicket : utxoDatum.fields[3],
            lotteryMinimumHash : utxoDatum.fields[4],
            lotteryTickets : utxoDatum.fields[5],
            lotteryIntervals : utxoDatum.fields[6]
          }
          const tokenName = ticketAssets[2].slice(56)
          const tokenNameBytes = helios.hexToBytes(tokenName)
          console.log('tokenName(Bytes):', tokenNameBytes)
          //sha2_256 (appendByteString ticketName $ consByteString soldTickets raffleSeed)
          const pseudoRandomNumber = [Number(newDatum.lotterySoldTicket)].concat(helios.hexToBytes(newDatum.lotteryRandomSeed))
          console.log('Pseudo Random number(Bytes) :',pseudoRandomNumber)
          const claimMinimunHashBytes = helios_internal.Crypto.sha2_256(tokenNameBytes.concat(pseudoRandomNumber))
          console.log('claimMinimumHashBytes(Bytes):', claimMinimunHashBytes)
          const claimMinimumHash = helios.bytesToHex(claimMinimunHashBytes)
          console.log('claimMinimumHash(Hex) :',claimMinimumHash )

          const newLotteryDatum = new
                    Constr (0, [
                        newDatum.lotteryTicketPrice,
                        newDatum.lotteryRandomSeed,
                        newDatum.lotteryMaxTicket,
                        newDatum.lotterySoldTicket,
                        claimMinimumHash,
                        newDatum.lotteryTickets,
                        newDatum.lotteryIntervals
                    ])
          console.log('new script datum(LotteryDatum): ', newLotteryDatum)
          const datum = Data.to(newLotteryDatum)
          console.log('new script datum(CBOR):', datum)
          const validatorRedeemerData = new Constr (1,[tokenName])
          console.log('validatorRedeemerData(Claim) :', validatorRedeemerData)
          const validatorRedeemer = Data.to(validatorRedeemerData) // Claim redeemer
          console.log('validatorRedeemer(Claim(CBOR)) : ', validatorRedeemer)
          
          try {
            const tx = await lucid.newTx()
                       .collectFrom([scriptUtxos[0]],validatorRedeemer)
                       .attachSpendingValidator(lotteryValidator)
                       .payToContract(lotteryValidatorAdd, datum,scriptUtxos[0].assets)
                       .validTo(Date.now() + toMiliseconds(100))
                       .complete()
            const signedTx = await tx.sign().complete()
            const txHash = signedTx.submit()
            console.log('Transaction submited:', txHash)
          } catch (err) {
            alert(err)
          }

        }
      } else {
        alert('no utxos found in loterry validator address : ' + lotteryValidatorAdd)
      }
    }
  }

  const close = async () => {
    if (lucid){
      console.log("Closing")
      const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
      const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
      const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
      const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
      const walletUtxos = await lucid.wallet.getUtxos()
      console.log(walletUtxos)
      const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                          .filter(asset => asset.includes(mintTicketPolicyId))
      console.log('tickets found in wallet:', ticketAssets.map(asset =>(asset.slice(56))))

      
      if (walletUtxos.length && scriptUtxos.length > 0) {
        if (scriptUtxos[0].datumHash) {
          const utxoDatum = Data.from (await lucid.datumOf(scriptUtxos[0])) // from CBOR to PlutusData/Json
          console.log('script datum found(LotteryDatum) : ', utxoDatum)
          const tokenName = ticketAssets[2].slice(56)
          const validatorRedeemerData = new Constr (2,[tokenName])
          console.log('validatorRedeemerData(Claim) :', validatorRedeemerData)
          const validatorRedeemer = Data.to(validatorRedeemerData) // Close redeemer
          console.log('validatorRedeemer(Claim(CBOR)) : ', validatorRedeemer)
          
          try {
            const tx = await lucid.newTx()
                       .collectFrom([scriptUtxos[0]].concat(walletUtxos),validatorRedeemer)
                       .attachSpendingValidator(lotteryValidator)
                       .validTo(Date.now() + toMiliseconds(1000))
                       .complete()
            const signedTx = await tx.sign().complete()
            const txHash = signedTx.submit()
            console.log('Transaction submited:', txHash)
          } catch (err) {
            alert(err)
          }

        }
      } else {
        alert('no utxos found in loterry validator address : ' + lotteryValidatorAdd)
      }
    }
  }

// <input  name="ticket price" defaultValue={ticketPrice} onChange={e => setTicketPrice(parseInt (e.target.value))} />
  return (
    <div className="px-10">
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/">
            <button className="btn normal-case text-xl"> Home </button>
          </Link>
        </div>
        
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div>Address: {walletStore.address}</div>
      <div className='m-10'>
        <p>
        </p>

      </div>
      <div className="mx-40 my-10">
        <button className="btn btn-secondary m-5" onClick={() => { mintRaffle() }}>Mint Raffle</button>
        <button className="btn btn-secondary m-5" onClick={() => { burnRaffle() }}>Burn Raffle</button>
        <button className="btn btn-secondary m-5" onClick={() => { burnTickets() }}>Burn Tickets</button>
        <button className="btn btn-secondary m-5" onClick={() => { getUtxos() }}>Get Utxo</button>
        <button className="btn btn-secondary m-5" onClick={() => { startRaffle() }}>Start Raffle</button>
        <button className="btn btn-secondary m-5" onClick={() => { buyTicket() }}>Buy Ticket</button>
        <button className="btn btn-secondary m-5" onClick={() => { claim() }}>Claim</button>
        <button className="btn btn-secondary m-5" onClick={() => { close() }}>Close</button>
      </div>
    </div>
  )
}

export default Raffle
