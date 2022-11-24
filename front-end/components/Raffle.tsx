import type { NextPage } from 'next'
import { useStoreActions, useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, Script } from 'lucid-cardano'
import { burnRaffle, buyTicket, claim, closeRaffle, mintRaffle, startRaffle, testEndpoint } from '../endpoints/RaffleEndpoints'
import { mintTicketValidator } from '../utils/validators'
import TicketGrid from './TicketGrid'
import WalletConnect from './WalletConnect'

const Raffle: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)

  //const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  // const [script, setScript] = useState<SpendingValidator>()
  // const [scriptAddress, setScriptAddress] = useState("")
  const [rafflePolicyId, setRafflePolicyId] = useState("")
  const [rafflePolicy, setRafflePolicy] = useState<Script>({ type: "Native", script: "" })
  // const [ticketPrice, setTicketPrice] = useState(10000000)
  // const [randomSeed, setRandomSeed ] = useState("adadadsds")
  // const [maxTicket, setMaxTicket] = useState(3)
  // const [minimumHash, setMinimumHash] = useState("")
  // const [tickets, setTickets] = useState([])
  const [ticketAssets, setTicketAssets] = useState<String[]>([])
  const [isWalletEnabled, setIsWalletEnabled] = useState(false)

  const checkWalletStatus = async () => {
    if (await window.cardano[walletStore.name.toLowerCase()]?.isEnabled()){
      setIsWalletEnabled(true)
    }else{
      setIsWalletEnabled(false)
    }
  }
  useEffect(() => {
    console.log(isWalletEnabled)
    checkWalletStatus()
    if (isWalletEnabled && !lucid) {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    } 
    if(lucid){
      getAssets(lucid)
      initRafflePolicy(lucid)
    }
    
  }, [lucid, isWalletEnabled])


  /**
  * One shot function to Initialize raffle policy.
  */
  const initRafflePolicy = async (lucid: Lucid) => {
    const { paymentCredential } = lucid.utils.getAddressDetails(await lucid.wallet.address());
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

  const getAssets = async (lucid: Lucid) => {
    const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
    const walletUtxos = await lucid.wallet.getUtxos()
    const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
      .filter(asset => asset.includes(mintTicketPolicyId))
    console.log(ticketAssets)
    setTicketAssets(ticketAssets)
  }

  if (!lucid) {
    return null
  }

  return (
    <div className="px-100">
      <div className='flex flex-row justify-evenly'>
        <div className='flex justify-start flex-col'>
          <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { mintRaffle(lucid, rafflePolicy) }}>Mint Raffle</button>
        </div>
        <div className='flex justify-start flex-col'>
          <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { startRaffle(lucid, rafflePolicy) }}>Start Raffle</button>
          <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full m-5" />
          <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full m-5" />
          <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full m-5" />
          <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full m-5" />
        </div>
        <div className='flex justify-start flex-col'>
          <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { buyTicket(lucid, rafflePolicy) }}>Buy Ticket</button>
          <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { claim(lucid, rafflePolicy) }}>Claim</button>
          <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { closeRaffle(lucid, rafflePolicy) }}>Close</button>
        </div>
        <div className='flex justify-start flex-col'>
          <button className="btn btn-outline btn-accent btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { burnRaffle(lucid, rafflePolicy) }}>Burn Raffle</button>
          <button className="btn btn-outline btn-accent btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { testEndpoint(lucid) }}>Test Endpoint</button>
        </div>
      </div>
      <div>
        <TicketGrid assets={ticketAssets} />
      </div>

    </div>
  )
}

export default Raffle
