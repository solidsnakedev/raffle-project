import { Lucid, Script } from 'lucid-cardano'
import React, { useState } from 'react'
import { closeRaffle } from '../endpoints/endpoints'

interface Props {
    lucid: Lucid,
    policy: Script
}

export const Close = ({ lucid, policy }: Props) => {
    const [error, setError] = useState("")



    const handleClick = async () => {
        const result = await closeRaffle(lucid, policy)
        result ? setError(result) : setError("")
    }
    return (
        <div className='flex flex-col justify-center items-center'>
             <div className='my-5'>
                <label className='label'>
                    <span className='label-text'>Ticket Name</span>
                </label>
                <input
                    type="text"
                    inputMode='decimal'
                    className="input input-bordered input-primary w-full"
                // value={ticketPrice}
                // onChange={(e) => setTicketPrice(Number(e.target.value.replace(/\D/g, '')))}
                />
            </div>
            <div className='my-5'>
                <label className='label'>
                    <span className='label-text'>Policy ID</span>
                </label>
                <input
                    type="text"
                    inputMode='decimal'
                    className="input input-bordered input-primary w-full"
                // value={ticketPrice}
                // onChange={(e) => setTicketPrice(Number(e.target.value.replace(/\D/g, '')))}
                />
            </div>


            <button
                className="btn btn-outline btn-primary btn-xs sm:btn-sm md:btn-md lg:btn-lg m-5"
                onClick={() => handleClick()}>Claim
            </button>
            {error &&
                <div className="alert alert-error shadow-lg">
                    <div>
                        <svg xmlns="http://www.w3.org/2000/svg" className="stroke-current flex-shrink-0 h-6 w-6" fill="none" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" /></svg>
                        <span>{error}</span>
                    </div>
                </div>

            }
        </div>
    )
}
