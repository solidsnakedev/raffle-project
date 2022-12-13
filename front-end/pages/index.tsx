import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import Link from 'next/link'
import Raffle from '../components/Raffle'

const Home: NextPage = () => {

  return (
    <div className="px-10">
      <Head>
        <title>Cardano Raffle</title>
        <meta name="description" content="Generated by create next app" />
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/" className="btn btn-ghost normal-case text-xl">Raffle</Link>
        </div>
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div className="mx-40 my-10">
        <Raffle />
      </div>
    </div>
  )
}

export default Home
