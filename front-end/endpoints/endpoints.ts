import { mintTicketValidator, lotteryValidator } from '../utils/validators'
import {exportedForTesting as helios_internal} from '@hyperionbt/helios'
import * as helios from '@hyperionbt/helios'
import { Lucid, Lovelace, Constr, Data, utf8ToHex, Script, Unit, Assets } from 'lucid-cardano'

  /**
    * Convert seconds to miliseconds.
    */
const toMilliseconds = (seconds : number) => {
    return Math.floor(seconds * 1000)
  }


export const testEndpoint = async (lucid: Lucid ): Promise<void> => {
    if (lucid) {
      console.log(await lucid.provider.getProtocolParameters())
      console.log(helios.bytesToHex(helios_internal.Crypto.sha2_256(helios_internal.stringToBytes("asda"))))

      console.log(lucid.utils.unixTimeToSlot(Date.now() + 1000000))
      console.log(helios.deserializeUplc(`{"type":"PlutusScriptV1", "cborHex": "${lotteryValidator.script}"}`).toString())

    }
  }

export const mintRaffle = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid) {
        console.log("minting raffle")
        console.log('raffle name : ', utf8ToHex("RaffleNFT #1"))
        const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
        const unit = rafflePolicyId + utf8ToHex("RaffleNFT #1");
        console.log('Raffle Unit : ', unit)
        
        const asset = {[unit]: BigInt(1)}
        console.log('Asset :', asset)

        const tx = await lucid.newTx().
                         mintAssets(asset).
                         validTo(Date.now()+toMilliseconds(100)).
                         attachMintingPolicy(rafflePolicy).
                         complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submitted:', txHash)
    }
  }

export const burnRaffle = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid) {
        console.log("burning raffle")
        const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
        const unit = rafflePolicyId + utf8ToHex("RaffleNFT #1");
        console.log('Raffle Unit : ', unit)
    
        const asset = {[unit]: BigInt(-1)}
        console.log('Asset :', asset)

        const tx = 
            await lucid.newTx().
            mintAssets(asset).
            validTo(Date.now()+ toMilliseconds(100)).
            attachMintingPolicy(rafflePolicy).
            complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log('Transaction submitted:', txHash)
    }
  }

  export const startRaffle = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid) {
        console.log("starting raffle")
        const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
        console.log('raffle policy id : ', rafflePolicyId)
        const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
        console.log('lottery validator address : ', lotteryValidatorAdd)
        console.log('mint ticket validator address : ', lucid.utils.validatorToAddress(mintTicketValidator))
        const ticketPrice = 10000000;
        const randomSeed = "adadadsds"
        const maxTickets = 3;
        const soldTickets = 0
        const minimumHash = "";
        const ticketList :string[] = [];
        // [buy period] | [claim period] | [close period]
        // ------------ 2 -------------- 4 --------------
        const intervals = new Constr (0,[
            BigInt(Date.now()+ toMilliseconds(600)), // 10 minutes
            BigInt(Date.now()+ toMilliseconds(1200)) //  20 minutes
        ])
        
        const lotteryDatum =
        new Constr (0, [
            BigInt(ticketPrice),
            utf8ToHex(randomSeed),
            BigInt(maxTickets),
            BigInt(soldTickets),
            utf8ToHex(minimumHash),
            ticketList,
            intervals
        ])
        console.log('lottery datum (PlutusData) : ', lotteryDatum)

        const datum = Data.to(lotteryDatum) // serialize to CBOR
        console.log('lottery datum (CBOR) : ', datum)
        
        //console.log(lucid.utils.unixTimeToSlot(Date.now()))
        const unit : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
        const asset = {[unit]: BigInt(1)}

        const tx = await
            lucid.newTx().
            mintAssets(asset).
            validTo(Date.now()+ toMilliseconds(100)).
            attachMintingPolicy(rafflePolicy).
            payToContract(lotteryValidatorAdd, datum, {[unit]: BigInt(1)}).
            complete()
    
        const signedTx = await
            tx.sign().
            complete()
    
        const txHash = signedTx.submit()
        console.log('Transaction submitted:', txHash)

    }
  }

export const buyTicket = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid){
      const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
      console.log('lottery validator address : ', lotteryValidatorAdd)
      const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
      const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
      const assetRaffle = {[unitRaffle]: BigInt(1)}
      const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
      const walletUtxos = await lucid.wallet.getUtxos()
      
      if (!walletUtxos.length) { return "wallet error"}
      if (!scriptUtxos.length) { return "script error"}

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
                  ticketPrice : utxoDatum.fields[0],
                  randomSeed : utxoDatum.fields[1],
                  maxTickets : utxoDatum.fields[2],
                  soldTickets : utxoDatum.fields[3],
                  minimumHash : utxoDatum.fields[4],
                  ticketList : utxoDatum.fields[5],
                  intervals : utxoDatum.fields[6]
              }
              const newLotteryDatum = new
                  Constr (0, [
                      newDatum.ticketPrice,
                      newDatum.randomSeed,
                      newDatum.maxTickets,
                      newDatum.soldTickets + BigInt(1),
                      newDatum.minimumHash,
                      newDatum.ticketList.concat(hashedTokenName),
                      newDatum.intervals
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
              // TODO: set a proper ticket price, it's hardcoded for now
              const newLovelace : Lovelace = BigInt(10000000)
              console.log('mintingRedeemer(CBOR) : ', mintingRedeemer)
              const currentLovelace = scriptUtxos[0].assets['lovelace']
              const newBalance : Assets = { ...scriptUtxos[0].assets, lovelace : currentLovelace + newLovelace}
              console.log(newBalance)
              try {
              // TODO: use txBuilderConfig to calculate minfee manually, remember to convert tx to bytes and apply fee formula with minFeeA and minFeeB
                const tx = await lucid.newTx()
                            .collectFrom([scriptUtxos[0],walletUtxos[0]],validatorRedeemer)
                            .attachSpendingValidator(lotteryValidator)
                            .attachMintingPolicy(mintTicketValidator)
                            .mintAssets(assetTicket, mintingRedeemer)
                            .payToContract(lotteryValidatorAdd, datum, newBalance )
                            .validTo(Date.now() + toMilliseconds(100))
                            .complete()
                console.log(await lucid.provider.getProtocolParameters())
                const signedTx = await tx.sign().complete()
                const txHash = await signedTx.submit()
                console.log('Transaction submitted:', txHash)
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
  }

export const claim = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid){
      console.log("Claiming")
      const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
      const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
      const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
      const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
      const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
      const walletUtxos = await lucid.wallet.getUtxos()
      console.log(walletUtxos)
      const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                          .filter(asset => asset.includes(mintTicketPolicyId))
      console.log('tickets found in wallet:', ticketAssets)

      if (!walletUtxos.length) { return "wallet error"}
      if (!scriptUtxos.length) { return "script error"}

      if (scriptUtxos[0].datumHash) {
        const utxoDatum = Data.from (await lucid.datumOf(scriptUtxos[0])) // from CBOR to PlutusData/Json
        console.log('script datum found(LotteryDatum) : ', utxoDatum)
        const newDatum = {
          ticketPrice : utxoDatum.fields[0],
          randomSeed : utxoDatum.fields[1],
          maxTickets : utxoDatum.fields[2],
          soldTickets : utxoDatum.fields[3],
          minimumHash : utxoDatum.fields[4],
          ticketList : utxoDatum.fields[5],
          intervals : utxoDatum.fields[6]
        }
        const tokenName = ticketAssets[2].slice(56)
        const tokenNameBytes = helios.hexToBytes(tokenName)
        console.log('tokenName(Bytes):', tokenNameBytes)
        //sha2_256 (appendByteString ticketName $ consByteString soldTickets raffleSeed)
        const pseudoRandomNumber = [Number(newDatum.soldTickets)].concat(helios.hexToBytes(newDatum.randomSeed))
        console.log('Pseudo Random number(Bytes) :',pseudoRandomNumber)
        const claimMinimumHashBytes = helios_internal.Crypto.sha2_256(tokenNameBytes.concat(pseudoRandomNumber))
        console.log('claimMinimumHashBytes(Bytes):', claimMinimumHashBytes)
        const claimMinimumHash = helios.bytesToHex(claimMinimumHashBytes)
        console.log('claimMinimumHash(Hex) :',claimMinimumHash )

        const newLotteryDatum = new
                  Constr (0, [
                      newDatum.ticketPrice,
                      newDatum.randomSeed,
                      newDatum.maxTickets,
                      newDatum.soldTickets,
                      claimMinimumHash,
                      newDatum.ticketList,
                      newDatum.intervals
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
                      .collectFrom([scriptUtxos[0]].concat(walletUtxos),validatorRedeemer)
                      .attachSpendingValidator(lotteryValidator)
                      .payToContract(lotteryValidatorAdd, datum,scriptUtxos[0].assets)
                      .validTo(Date.now() + toMilliseconds(100))
                      .complete()
          const signedTx = await tx.sign().complete()
          const txHash = await signedTx.submit()
          console.log('Transaction submitted:', txHash)
        } catch (err) {
          alert(err)
        }

      }

    }
  }

export const closeRaffle = async (lucid : Lucid, rafflePolicy: Script) => {
    if (lucid){
      console.log("Closing")
      const lotteryValidatorAdd = lucid.utils.validatorToAddress(lotteryValidator)
      const rafflePolicyId = lucid.utils.mintingPolicyToId(rafflePolicy);
      const unitRaffle : Unit = rafflePolicyId + utf8ToHex ("RaffleNFT #1")
      const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
      const scriptUtxos = await lucid.utxosAtWithUnit(lotteryValidatorAdd, unitRaffle);
      const walletUtxos = await lucid.wallet.getUtxos()
      console.log('wallet utxos', walletUtxos)
      const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                          .filter(asset => asset.includes(mintTicketPolicyId))
      console.log('tickets found in wallet:', ticketAssets.map(asset =>(asset.slice(56))))

      if (!walletUtxos.length) { return "wallet error"}
      if (!scriptUtxos.length) { return "script error"}

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
                      .validTo(Date.now() + toMilliseconds(1000))
                      .complete()
          const signedTx = await tx.sign().complete()
          const txHash = await signedTx.submit()
          console.log('Transaction submitted:', txHash)
        } catch (err) {
          alert(err)
        }

        }
    }
  }

//   const burnTickets = async () => {
//     if (lucid) {
//       console.log("burning tickets")

//       const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
//       const walletUtxos = await lucid.wallet.getUtxos()
//       console.log(walletUtxos)
//       const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
//                           .filter(asset => asset.includes(mintTicketPolicyId))
//       console.log(ticketAssets)
//       const assetsToBurn = ticketAssets.reduce((result, item) => {
//         return { ...result, [item]: BigInt(-1)}
//       },{})

//       console.log(assetsToBurn)

//       const assetsToBurnList = ticketAssets.map(asset => { return {[asset]: BigInt(1)}})
//       console.log(assetsToBurnList)
      
//       const walletTxHash = walletUtxos[0].txHash
//       const walletOutputIndex = walletUtxos[0].outputIndex
//       const txHashBytes = helios.hexToBytes(walletTxHash) // helios
//       console.log('txHashBytes : ', txHashBytes)
//       //const id = new Uint8Array([walletOutputIndex])  // lucid
//       const id = walletOutputIndex
//       //const tokenName = concat(id,txhash) // lucid
//       const tokenName = [id].concat(txHashBytes) // concatenate uint8 numbers
//       console.log('concat : ', tokenName)
//       //const sha = sha256(tokenName) // sha256 from lucid
//       const hashedTokenNameBytes = helios_internal.Crypto.sha2_256(tokenName) //sha256 from helios
//       console.log('hashedTokenNameBytes : ', hashedTokenNameBytes)
//       //console.log('hex : ', toHex(new Uint8Array(sha))) // Lucid
//       const hashedTokenName = helios.bytesToHex(hashedTokenNameBytes)
//       console.log('hashedTokenName : ', hashedTokenName)

//       const walletTxHashData = new Constr (0, [walletUtxos[0].txHash]) // refers to TxId Constructor in PlutusTx
//       const mintingRedeemerData = new Constr (0, [ walletTxHashData , BigInt(walletUtxos[0].outputIndex)]) // refers TxOutRef constructor in PlutusTx
//       const mintingRedeemer = Data.to(mintingRedeemerData) // TxOutRef to CBOR

//       const unitTicket = mintTicketPolicyId + hashedTokenName
//       const assetTicket = {[unitTicket]: BigInt(1)}
//       const tx = 
//           await lucid.newTx().
//           mintAssets(assetTicket, mintingRedeemer).
//           attachMintingPolicy(mintTicketValidator).
//           validTo(Date.now()+ toMiliseconds(100)).
//           complete();

//       const signedTx = await tx.sign().complete();
//       const txHash = await signedTx.submit();
//       console.log('Transaction submited:', txHash)
//     }
//   }
