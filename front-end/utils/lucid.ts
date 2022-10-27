import { Blockfrost, Lucid, Network } from 'lucid-cardano';

export const networkParams: {apiUrl: string, apiKey: string, network: Network} = {
    apiUrl: 'https://cardano-preprod.blockfrost.io/api/v0' ,
    apiKey: 'preprodOr3zZOkFc8Sqa5sp3aa9oGTb1wxulzhy' ,
    network: 'Preprod'
}

const initLucid = async (wallet: string) => {
    const api = (await window.cardano[
        wallet.toLowerCase()
    ].enable())

    const lucid = await Lucid.new(
        new Blockfrost(networkParams.apiUrl, networkParams.apiKey), networkParams.network)
    lucid.selectWallet(api)
    //setLucid(lucid)
    return lucid;
}

export default initLucid;