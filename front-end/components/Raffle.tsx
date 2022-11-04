import type { NextPage } from 'next'
import { useStoreActions, useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, Script} from 'lucid-cardano'
import { burnRaffle, buyTicket, claim, closeRaffle, mintRaffle, startRaffle, testEndpoint } from '../endpoints/RaffleEndpoints'
import { mintTicketValidator } from '../utils/validators'
import TicketGrid from './TicketGrid'

const Raffle: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  //const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  // const [script, setScript] = useState<SpendingValidator>()
  // const [scriptAddress, setScriptAddress] = useState("")
  const [rafflePolicyId, setRafflePolicyId] = useState("")
  const [rafflePolicy, setRafflePolicy]= useState<Script>({type: "Native", script:""})
  // const [ticketPrice, setTicketPrice] = useState(10000000)
  // const [randomSeed, setRandomSeed ] = useState("adadadsds")
  // const [maxTicket, setMaxTicket] = useState(3)
  // const [minimumHash, setMinimumHash] = useState("")
  // const [tickets, setTickets] = useState([])
  const [ticketAssets, setTicketAssets] = useState<String[]>([])

  useEffect(() => {
    if (lucid) {
      getAssets(lucid)
      initRafflePolicy(lucid)
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])

  /**
  * One shot function to Initialize raffle policy.
  */
  const initRafflePolicy = async (lucid : Lucid) => {
    const {paymentCredential} = lucid.utils.getAddressDetails(await lucid.wallet.address());
    const mintingPolicy = lucid.utils.nativeScriptFromJson(
      {
          type: "sig",
          keyHash: paymentCredential?.hash
      }
    )
    console.log('minting policy : ', mintingPolicy)
    const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);
    setRafflePolicyId(policyId)
    setRafflePolicy(mintingPolicy)
  }

  const getAssets = async (lucid : Lucid) =>{
    const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
    const walletUtxos = await lucid.wallet.getUtxos()
    const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
                        .filter(asset => asset.includes(mintTicketPolicyId))
    console.log(ticketAssets)
    setTicketAssets(ticketAssets)
  }

  if (lucid){
    return (
      <div className="px-10">
        <div className="mx-40 my-10">
          <button className="btn btn-secondary m-5" onClick={() => { mintRaffle(lucid, rafflePolicy) }}>Mint Raffle</button>
          <button className="btn btn-secondary m-5" onClick={() => { burnRaffle(lucid, rafflePolicy) }}>Burn Raffle</button>
          <button className="btn btn-secondary m-5" onClick={() => { startRaffle(lucid, rafflePolicy) }}>Start Raffle</button>
          <button className="btn btn-secondary m-5" onClick={() => { buyTicket(lucid, rafflePolicy) }}>Buy Ticket</button>
          <button className="btn btn-secondary m-5" onClick={() => { claim(lucid, rafflePolicy) }}>Claim</button>
          <button className="btn btn-secondary m-5" onClick={() => { closeRaffle(lucid, rafflePolicy) }}>Close</button>
          <button className="btn btn-secondary m-5" onClick={() => { testEndpoint(lucid) }}>Test Endpoint</button>
        </div>
        <TicketGrid assets={ticketAssets} />
      </div>
    )
  }
  else {
    return(
      <div>
      </div>
    )
  }
}

export default Raffle
