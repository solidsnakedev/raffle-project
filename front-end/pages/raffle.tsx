import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import { Lucid, TxHash, Lovelace, Constr, SpendingValidator, Data, utf8ToHex, Script, Datum, datumJsonToCbor, toHex, sha256, concat } from 'lucid-cardano'
import * as helios from '@hyperionbt/helios'
import {exportedForTesting as helios_internal} from '@hyperionbt/helios'
import { min_fee } from 'lucid-cardano/types/src/core/wasm_modules/cardano_multiplatform_lib_web/cardano_multiplatform_lib'
import { sign } from 'crypto'
import { setPriority } from 'os'
import { mintTicketValidator, lotteryValidator } from '../utils/validators'


const Raffle: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")
  const [rafflePolicyId, setRafflePolicyId] = useState("")
  const [ticketPrice, setTicketPrice] = useState(10000000)
  const [randomSeed, setRandomSeed ] = useState("adadadsds")
  const [maxTicket, setMaxTicket] = useState(3)
  const [minimumHash, setMinimumHash] = useState("")
  const [tickets, setTickets] = useState([])

  useEffect(() => {
    if (lucid) {
      const thisScript: SpendingValidator = {
        type: "PlutusV1",
        script: JSON.parse(
          helios.Program.new(`
          spending matching_pubKeyHash
          struct Datum {
              owner: PubKeyHash
          }
          struct Redeemer {
              owner: PubKeyHash
          }
          func main(datum : Datum, redeemer: Redeemer) -> Bool {datum.owner == redeemer.owner}
      `).compile().serialize(),
        ).cborHex,
      };
      setScript(thisScript)
      setScriptAddress(lucid.utils.validatorToAddress(thisScript))
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])
  
  const toMiliseconds = (seconds : number) => {
    return Math.floor(seconds * 1000)
  }

  const getUtxos = async () => {
    if (lucid) {
      const utxos = await lucid.utxosAt(scriptAddress);
      if (utxos.length > 0) {
        utxos.map(async (value: any) => {
          console.log(value.txHash)
        });
      } else {
        console.log("no utxos in script")
      }


      console.log(helios.bytesToHex(helios_internal.Crypto.sha2_256(helios_internal.stringToBytes("asda"))))

      console.log(lucid.utils.unixTimeToSlot(Date.now() + 1000000))

      // amount.map(async (asset: any) => {
      //console.log(utxos);
    }
  }

  const mintRaffle = async () => {
    if (lucid) {
        console.log("minting raffle")
        const {paymentCredential} = lucid.utils.getAddressDetails(await lucid.wallet.address());

        // const mintingPolicy = lucid.utils.nativeScriptFromJson(
        //     {
        //         type: "all",
        //         scripts: [
        //             { type: "sig", keyHash: paymentCredential?.hash },
        //             {
        //                 type: "before",
        //                 slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),
        //             },
        //         ],
        //     },
        // )

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
        console.log('policy id : ', policyId)
        setRafflePolicyId(policyId)


        const unit = policyId + utf8ToHex("RaffleNFT #1");

        console.log('Raffle NFT : ', unit)
        
        const asset = {[unit]: BigInt(1)}

        console.log('Asset :', asset)
        const tx = await lucid.newTx().
                         mintAssets(asset).
                         validTo(Date.now()+toMiliseconds(100)).
                         attachMintingPolicy(mintingPolicy).
                         complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submited:', txHash)

    }
  }


  const burnRaffle = async () => {
    if (lucid) {
        console.log("minting raffle")
        const {paymentCredential} = lucid.utils.getAddressDetails(await lucid.wallet.address());

        // const mintingPolicy = lucid.utils.nativeScriptFromJson(
        //     {
        //         type: "all",
        //         scripts: [
        //             { type: "sig", keyHash: paymentCredential?.hash },
        //             {
        //                 type: "before",
        //                 slot: 11033009,
        //             },
        //         ],
        //     },
        // )

        const mintingPolicy = lucid.utils.nativeScriptFromJson(
            {
                type: "sig",
                keyHash: paymentCredential?.hash
            }
        )

        //const policyId = 'ba6bdf86608cb4cd0d2bfaf7bfdc68720e5e88b655e4cd4302f4758d'
        console.log('policy id : ', rafflePolicyId)

        const unit = rafflePolicyId + utf8ToHex("RaffleNFT #1");

        console.log('Raffle NFT : ', unit)
        
        const asset = {[unit]: BigInt(-1)}

        console.log('Asset :', asset)
        const tx = 
            await lucid.newTx().
            mintAssets(asset).
            validTo(Date.now()+ toMiliseconds(100)).
            attachMintingPolicy(mintingPolicy).
            complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submited:', txHash)
    }
  }

  const startRaffle = async () => {
    if (lucid) {
        console.log("starting raffle")
        console.log(rafflePolicyId)
        const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
        console.log(lotteryValidatorAdd)
        console.log(lucid.utils.validatorToAddress(mintTicketValidator))
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
        console.log(lotteryDatum)

        const datum = Data.to(lotteryDatum) // serialise to CBOR
        console.log(datum)
        
        //console.log(lucid.utils.unixTimeToSlot(Date.now()))
        const unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
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
            console.log('Raffle not found in wallet address')
        }

    }
  }

  const buyTicket = async () => {
    if (lucid){
        const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
        console.log('lottery script address : ', lotteryValidatorAdd)
        const unitRaffle = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
        const assetRaffle = {[unitRaffle]: BigInt(1)}
        const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
        const walletUtxos = await lucid.wallet.getUtxos()

        if (walletUtxos.length && scriptUtxos.length > 0) {
            console.log('wallet txHash : ', walletUtxos[0].txHash)
            console.log('outputIndex', walletUtxos[0].outputIndex)
            const walletTxHash = walletUtxos[0].txHash
            const walletOutputIndex = walletUtxos[0].outputIndex
            //console.log(helios.bytesToHex([2]))
            //console.log(helios.bytesToHex([2,97,98,99]))
            //console.log(helios.bytesToHex([255]))
            //console.log(toHex(new Uint8Array([1])))
            //console.log(toHex(new Uint8Array([1])) + '34fc86e20ea0a1984e27c7594853bb2b25f32a61d0448fed85884d6f3638784b')
            //console.log(helios_internal.Crypto.sha2_256())
            //const txhash = new Uint8Array (helios_internal.stringToBytes(walletTxHash)) // lucid
            //const txHashBytes = helios_internal.stringToBytes(walletTxHash) // helios
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

            console.log('utxo found in lottery validator address : ', scriptUtxos)
            if (scriptUtxos[0].datumHash) {
                const utxoDatum = Data.from (await lucid.datumOf(scriptUtxos[0])) // from CBOR to PlutusData/Json
                console.log('datum found : ', utxoDatum)
                const newDatum = {
                    lotteryTicketPrice : utxoDatum.fields[0],
                    lotteryRandomSeed : utxoDatum.fields[1],
                    lotteryMaxTicket : utxoDatum.fields[2],
                    lotterySoldTicket : utxoDatum.fields[3],
                    lotteryMinimumHash : utxoDatum.fields[4],
                    lotteryTickets : utxoDatum.fields[5],
                    lotteryIntervals : utxoDatum.fields[6]
                }
                console.log('new datum : ', newDatum)
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
                console.log('new lottery datum : ', newLotteryDatum)
                const datum = Data.to(newLotteryDatum)
                console.log('datum :', datum)
                const validatorRedeemer = Data.to(new Constr (0,[])) // Buy redeemer
                console.log('validatorRedeemer : ', validatorRedeemer)
                const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
                const unitTicket = mintTicketPolicyId + hashedTokenName
                const assetTicket = {[unitTicket]: BigInt(1)}
                console.log(assetTicket)
                const walletTxHashData = new Constr (0, [walletTxHash])
                const mintingRedeemerData = new Constr (0, [ walletTxHashData , BigInt(walletOutputIndex)]) // TxOutRef
                const mintingRedeemer = Data.to(mintingRedeemerData) // TxOutRef to CBOR
                console.log(mintingRedeemerData)
                const lovelace : Lovelace = BigInt(10000000)
                console.log(mintingRedeemer)
                // TOdo add pay to wallet when minting
                const tx = await lucid.newTx()
                           .collectFrom([scriptUtxos[0]],validatorRedeemer)
                           .attachSpendingValidator(lotteryValidator)
                           .attachMintingPolicy(mintTicketValidator)
                           .mintAssets(assetTicket, mintingRedeemer)
                           .payToContract(lotteryValidatorAdd, datum,{lovelace, [unitRaffle]: BigInt(1)})
                           .validTo(Date.now())
                           .complete()
                //console.log(tx)
                const signedTx = await tx.sign().complete()
                   
                const txHash = signedTx.submit()
                console.log('Transaction submited:', txHash)
            }
        }
        else {
            console.log('no utxos found in loterry validator address : ', lotteryValidatorAdd)
        }
        
  }}

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
        <button className="btn btn-secondary m-5" onClick={() => { getUtxos() }}>Get Utxo</button>
        <button className="btn btn-secondary m-5" onClick={() => { startRaffle() }}>Start Raffle</button>
        <button className="btn btn-secondary m-5" onClick={() => { buyTicket() }}>Buy Ticket</button>
      </div>
    </div>
  )
}

export default Raffle
