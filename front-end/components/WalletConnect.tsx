import { Lucid, Blockfrost, utf8ToHex, C } from "lucid-cardano";
import { useState, useEffect } from 'react';
import { useStoreActions, useStoreState } from "../utils/store";
import initLucid from "../utils/lucid";

const WalletConnect = () => {
    // const [availableWallets, setAvailableWallets] = useState<string[]>([])
    const walletStore = useStoreState(state => state.wallet)
    const setWallet = useStoreActions(actions => actions.setWallet)
    const availableWallets = useStoreState(state => state.availableWallets)
    const setAvailableWallets = useStoreActions(actions => actions.setAvailableWallets)
    
    const [connectedAddress, setConnectedAddress] = useState("")
    const [mounted, setMounted] = useState(false);
    
    const loadWalletSession = async () => {
        const isWalletEnabled  = await window.cardano[walletStore.name.toLowerCase()]?.isEnabled() ?? false
        if (!isWalletEnabled) {
            const walletStoreObj = { connected: false, name: '', address: '' }
            setWallet(walletStoreObj)
        }else {

            walletConnected(walletStore.name)
        }
        // if (walletStore.connected &&
        //     walletStore.name &&
        //     window.cardano &&
        //     (await window.cardano[walletStore.name.toLowerCase()].enable())
        // ) {
        //     walletConnected(walletStore.name)
        // }
    }

    const walletConnected = async (wallet: string) => {
        console.log('walletConnected')
        const addr = await (await initLucid(wallet)).wallet.address()
        const walletStoreObj = { connected: true, name: wallet, address: addr }
        setConnectedAddress(addr)
        setWallet(walletStoreObj)
    }

    const selectWallet = async (wallet: string) => {
        if (
            window.cardano &&
            (await window.cardano[wallet.toLocaleLowerCase()].enable())
        ) {
            await walletConnected(wallet)
            window.location.reload()
        }
    }

    useEffect(() => {
        if (window.cardano) {
            const walletList = Object.keys(window.cardano).filter((walletName)=>
            window.cardano[walletName].icon &&
            walletName !== "ccvault" &&
            walletName !== "typhon"
            )
            setAvailableWallets(walletList)
            loadWalletSession()
            setMounted(true)
        }
    }, [])

    return (
        <>
            <div className="dropdown dropdown-end">
                <label tabIndex={0} className="btn m-1">{connectedAddress != "" ? 'Connected' : 'Connect'}</label>
                <ul tabIndex={0} className="dropdown-content menu p-2 shadow bg-base-300 rounded-box w-52">
                {availableWallets.map((wallet) =>
                        <li key={wallet} onClick={() => { selectWallet(wallet) }} >
                            <div className="flex flex-row justify-evenly">
                                <div className="basis-8">
                                <img src={window.cardano[wallet].icon} />
                                </div>
                                <div className="basis-1"> {wallet.charAt(0).toUpperCase() + wallet.slice(1)}</div>
                            </div>
                            {/* <a >
                                {wallet}
                                </a> */}
                        </li>
                    )}
                </ul>
            </div>
        </>
    )
}

export default WalletConnect;