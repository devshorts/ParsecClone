namespace BinaryCombinator

open System.IO
open Combinator

type Bit = 
    | One
    | Zero

type BitStream (state:byte[], bitOffset:int) =   
           
    let bitMasks = Seq.unfold (fun bitIndex -> Some((byte(pown 2 bitIndex), bitIndex), bitIndex + 1)) 0                           
                            |> Seq.take 8
                            |> Seq.toList
                            |> List.rev
    
    let byteToBitArray b = 
            List.map (fun (bitMask, bitPosition) -> 
                        if (b &&& bitMask) >>> bitPosition = byte(0) then Zero
                        else One) bitMasks

    let bytesToBits (bytes:byte[]) bitCount =
        let numBytesToFetch = (bitCount - 1) / 8

        bytes.[0..numBytesToFetch]   
            |> Array.toList
            |> List.map byteToBitArray
            |> List.collect id
            |> List.toArray
            |> fun i -> i.[0..bitCount - 1]

    let bitsToUInt (bits:Bit[])  = 
        let positions = Array.zip bits (Array.rev [|0..Array.length bits - 1|])

        Array.fold (fun acc (bit, index) -> 
                        match bit with 
                            | Zero -> acc
                            | One -> acc + (pown 2 index)) 0 positions

    member x.toI = x :> IStreamP<byte[], int>

    member x.bitOffset = bitOffset

    interface IStreamP<byte[], int>  with       
        member x.state = state     

        member x.consume count = 
            if count > (Array.length state) * 8 then 
                (None, x.toI)
            else
                let bits = bytesToBits state count

                (Some(bitsToUInt bits), new BitStream(state, count) :> IStreamP<byte[], int>)
            
        member x.skip count = 
            let (_, newState) = x.toI.consume count
            (Some(true), newState)
                    
        member x.backtrack () = () 

        member x.hasMore () = Array.length state <> 0 && (Array.length state) * 8 <> bitOffset

        member x.equals istream = true  
        
        member x.canConsume count =  if count < (Array.length state) * 8 then Some(count) else None
        
    