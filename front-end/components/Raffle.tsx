import type { NextPage } from 'next'
import { useStoreActions, useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, Script } from 'lucid-cardano'
import { burnRaffle, buyTicket, claim, closeRaffle, mintRaffle, startRaffle, testEndpoint } from '../endpoints/RaffleEndpoints'
import TicketGrid from './TicketGrid'
import { StartRaffle } from './StartRaffle'


const Raffle = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [lucid, setLucid] = useState<Lucid>()
  const [rafflePolicy, setRafflePolicy] = useState<Script>({ type: "Native", script: "" })
  const [whiteListed, setWhiteListed] = useState(false)

  const initRafflePolicy = async (lucid: Lucid) => {
    const { paymentCredential } = lucid.utils.getAddressDetails(await lucid.wallet.address());
    const policy = lucid.utils.nativeScriptFromJson(
      {
        type: "sig",
        keyHash: paymentCredential?.hash
      }
    )
    console.log('minting policy : ', policy)
    setRafflePolicy(policy)
  }

  const isWhiteListed = async () => {
    const result = await window.cardano[walletStore.name.toLowerCase()]?.isEnabled()
    result ? setWhiteListed(true) : setWhiteListed(false)
  }

  useEffect(() => {
    isWhiteListed()
    console.log('Raffle.tsx -> whiteListed', whiteListed)
    console.log('Raffle.tsx -> lucid', lucid)
    console.log('Raffle.tsx -> walletStore.connected', walletStore.connected)
    if (whiteListed && walletStore.connected && !lucid) {
      console.log("Raffle.tsx -> initLucid")
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
    if (lucid) {
      initRafflePolicy(lucid)
    }

  }, [lucid, walletStore.connected, whiteListed])

  if (!lucid) {
    return null
  }

  return (
    <div className="px-100">
      <div className='flex flex-row justify-evenly'>
        <StartRaffle lucid={lucid} policy={rafflePolicy} />
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
      <div className='m-10'>
        <TicketGrid lucid={lucid}/>
      </div>

    </div>
  )
}

export default Raffle
