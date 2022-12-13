import { Lucid, Script } from 'lucid-cardano'
import React, { useEffect } from 'react'
import { startRaffle } from '../endpoints/RaffleEndpoints'

interface Props{
    lucid: Lucid,
    policy : Script
}

export const StartRaffle = ({lucid, policy}:Props) => {

    useEffect(() => {
        console.log('policy', policy)
    }, [policy])
    
  return (
    <div className='flex justify-start flex-col'>
    <button className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5" onClick={() => { startRaffle(lucid, policy) }}>Start Raffle</button>
    <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full my-5" />
    <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full my-5" />
    <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full my-5" />
    <input type="text" placeholder="Type here" className="input input-bordered input-primary w-full my-5" />
  </div>
  )
}
