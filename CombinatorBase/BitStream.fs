namespace ParsecClone.BinaryCombinator 

open System.IO
open ParsecClone.CombinatorBase

type BitStream (state:byte[], bitOffset:int) =   
           
    member x.toI = x :> IStreamP<byte[], Bit[]>

    member x.bitOffset = bitOffset

    interface IStreamP<byte[], Bit[]>  with       
        member x.state = state     

        member x.consume count = 
            let lastBitPos = bitOffset + count

            if lastBitPos > (Array.length state) * 8 then 
                (None, x.toI)
            else
                let bits = bytesToBits state

                (Some(bits.[bitOffset..lastBitPos - 1]), new BitStream(state, lastBitPos) :> IStreamP<byte[], Bit[]>)
            
        member x.skip count = 
            let (_, newState) = x.toI.consume count
            (Some(true), newState)
                    
        member x.backtrack () = () 

        member x.hasMore () = 
            Array.length state <> 0 && (Array.length state) * 8 <> bitOffset

        member x.equals istream = true  
        
        member x.canConsume count =  
            if count + bitOffset <= (Array.length state) * 8 then Some(count) else None
        
    