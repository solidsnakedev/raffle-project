import { useEffect } from 'react'

type Props = {
    assets : String[]
}

const TicketGrid = ({assets}: Props) => {
    console.log('props', assets)
    return (
        <div>
            <h1 className="text-5xl font-bold mt-0 mb-6">Tickets</h1>
            <div className="flex flex-col w-full border-opacity-50">
                {assets.map(element => 
                    <div>
                        <div className='m-8 grid h-20 card bg-secondary rounded-box place-items-center'>
                            {element.slice(56)}
                        </div>
                        <div className="divider"></div>
                    </div>
                )}
            </div>
        </div>
    )
}

export default TicketGrid;