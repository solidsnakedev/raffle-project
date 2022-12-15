import type { NextPage } from 'next'
import { useStoreActions, useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, Script } from 'lucid-cardano'
import { burnRaffle, buyTicket, claim, closeRaffle, mintRaffle, startRaffle, testEndpoint } from '../endpoints/endpoints'
import TicketGrid from './TicketGrid'
import { StartRaffle } from './StartRaffle'
import { BuyTicket } from './BuyTicket'
import { StartRaffle2 } from './StartRaffle2'
import { Claim } from './Claim'
import { Close } from './Close'


const Raffle = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [lucid, setLucid] = useState<Lucid>()
  const [rafflePolicy, setRafflePolicy] = useState()
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

  if (!lucid || !rafflePolicy) {
    return null
  }

  return (
    <div className='flex flex-col justify-center items-center'>
      <div className='flex gap-2 m-5 flex-wrap'>
        <a href='#item1' className='btn'>Start Raffle</a>
        <a href='#item2' className='btn'>Buy Ticket</a>
        <a href='#item3' className='btn'>Claim</a>
        <a href='#item4' className='btn'>Close</a>
      </div>

      <div className='carousel carousel-center rounded-box bg-neutral max-w-xs'>
        <div id='item1' className='carousel-item justify-center w-full'>
          {/* <FormHero></FormHero> */}
          <StartRaffle2 lucid={lucid} policy={rafflePolicy} />
          {/* <h1>item1</h1> */}
        </div>
        <div id='item2' className='carousel-item justify-center w-full'>
          <BuyTicket lucid={lucid} policy={rafflePolicy} />
        </div>
        <div id='item3' className='carousel-item justify-center w-full'>
          <Claim lucid={lucid} policy={rafflePolicy}/>
        </div>
        <div id='item4' className='carousel-item justify-center w-full'>
          <Close lucid={lucid} policy={rafflePolicy}/>
        </div>
      </div>

      <div className='my-8'>
      <TicketGrid lucid={lucid}/>
      </div>
    </div>
    // <div>
    //   <div className="navbar bg-base-100 flex justify-evenly">
    //     <a className="btn btn-ghost normal-case text-xl">Start Raffle</a>
    //     <a className="btn btn-ghost normal-case text-xl">Buy Ticket</a>
    //     <a className="btn btn-ghost normal-case text-xl">daisyUI</a>
    //   </div>
    //   <div className='flex flex-col justify-evenly flex-wrap'>
    //     <StartRaffle lucid={lucid} policy={rafflePolicy} />
    //     <BuyTicket lucid={lucid} policy={rafflePolicy} />
    //     <div className='flex  '>
    //       <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { claim(lucid, rafflePolicy) }}>Claim</button>
    //       <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { closeRaffle(lucid, rafflePolicy) }}>Close</button>
    //     </div>
    //     <div className='flex '>
    //       <button className="btn btn-outline btn-accent btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { burnRaffle(lucid, rafflePolicy) }}>Burn Raffle</button>
    //       <button className="btn btn-outline btn-accent btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { testEndpoint(lucid) }}>Test Endpoint</button>
    //     </div>
    //   </div>
    //   <div className=''>
    //     <TicketGrid lucid={lucid} />
    //   </div>

    // </div>
  )
}

export default Raffle
