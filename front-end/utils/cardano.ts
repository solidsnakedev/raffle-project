import { networkParams } from "./lucid";

export const getAssets = async (address: string) => {
    var allNFTs: any = []
    var addressInfo = { nfts: allNFTs, balance: 0 }
    const data = await fetch(
        `${networkParams.apiUrl}/addresses/${address}`,
        {
            headers: {
                // Your Blockfrost API key
                project_id: `${networkParams.apiKey}` ,
                'Content-Type': 'application/json'
            }
        }
    ).then(res => res.json());
    
    console.log('printing data')
    console.log(data)
    if (data?.error) {
        // Handle error.
        console.log("error")
    }

    const amount = data['amount']
    if (amount.length > 0) {
        amount.map(async (asset: any) => {
            //var allNFTs = []
            if (asset.unit !== "lovelace") {
                const data = await fetch(
                    `${networkParams.apiUrl}/assets/${asset.unit}`,
                    {
                        headers: {
                            // Your Blockfrost API key
                            project_id: `${networkParams.apiKey}`,
                            'Content-Type': 'application/json'
                        }
                    }
                ).then(res => res.json());
                const meta = data['onchain_metadata'];
                if (meta && meta.image) {
                    allNFTs.push({ ...meta, assetId: data.asset })
                } else {
                       console.log("nometa", data)
                }
            } else if (asset.unit === 'lovelace') {
                console.log('just ada')
                console.log(asset.quantity)
                addressInfo.balance === asset.quantity
            }
        })
    }
    return { addressInfo }
}