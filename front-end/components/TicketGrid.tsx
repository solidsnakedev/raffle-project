import { Lucid, Script } from 'lucid-cardano'
import { useEffect, useState } from 'react'
import { mintTicketValidator } from '../utils/validators'

interface Props{
    lucid: Lucid
}


const TicketGrid = ({lucid}: Props) => {
    const [tickets, setTickets] = useState<string[]>([])

    useEffect(() => {
      const getAssets = async () => {
          const mintTicketPolicyId = lucid.utils.mintingPolicyToId(mintTicketValidator)
          const walletUtxos = await lucid.wallet.getUtxos()
          const ticketAssets = walletUtxos.map(utxo => (Object.keys(utxo.assets))).flat()
          .filter(asset => asset.includes(mintTicketPolicyId))
          console.log(ticketAssets)
          setTickets(ticketAssets)
      }
      
      getAssets()

    }, [])

    if (!tickets){
        return null
    }
    
    return (
        <div>
            <h1 className="text-5xl font-bold mt-0 mb-6">Tickets</h1>
            <div className="divider"></div> 
            <div className="flex flex-col w-full border-opacity-50">
                { tickets.map(element => 
                    <div key={element}>
                        <div className='flex items-center justify-evenly'>
                            <div >
                                <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="w-6 h-6">
                                    <path strokeLinecap="round" strokeLinejoin="round" d="M16.5 6v.75m0 3v.75m0 3v.75m0 3V18m-9-5.25h5.25M7.5 15h3M3.375 5.25c-.621 0-1.125.504-1.125 1.125v3.026a2.999 2.999 0 010 5.198v3.026c0 .621.504 1.125 1.125 1.125h17.25c.621 0 1.125-.504 1.125-1.125v-3.026a2.999 2.999 0 010-5.198V6.375c0-.621-.504-1.125-1.125-1.125H3.375z" />
                                </svg>
                            </div>
                            <h1>
                                {element.slice(56)}
                            </h1>
                        </div>
                        <div className="divider"></div>
                    </div>
                )}
            </div>
        </div>
    )
}

export default TicketGrid;