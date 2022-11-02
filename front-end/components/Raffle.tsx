import type { NextPage } from 'next'
import { useStoreActions, useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, SpendingValidator, Script} from 'lucid-cardano'
import { burnRaffle, buyTicket, claim, closeRaffle, mintRaffle, startRaffle, testEndpoint } from '../endpoints/RaffleEndpoints'

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

  useEffect(() => {
    if (lucid) {
      /**
      * One shot function to Initialize raffle policy.
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

// <input  name="ticket price" defaultValue={ticketPrice} onChange={e => setTicketPrice(parseInt (e.target.value))} />
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
    </div>
  )}
  else {
    return(
      <div className="alert alert-warning shadow-lg">
        <div>
          <svg xmlns="http://www.w3.org/2000/svg" className="stroke-current flex-shrink-0 h-6 w-6" fill="none" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" /></svg>
          <span>Warning: Connect Wallet!</span>
        </div>
      </div>
    )
  }
}

export default Raffle
