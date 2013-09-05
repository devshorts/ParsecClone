namespace ParsecClone.BinaryCombinator

open System.Reflection
open System.IO
open System.Runtime.InteropServices
open ParsecClone.CombinatorCS
open System
open System.Collections.Generic

[<AutoOpen>]
module ByteUtils = 
   
    type Bit = 
        | One
        | Zero
        override this.ToString() = 
            match this with 
                | One -> "1"
                | Zero -> "0"
        
    let bitToByte = function
                        | One -> byte(1)
                        | Zero -> byte(0)

    let bitMasks = Seq.unfold (fun bitIndex -> Some((byte(pown 2 bitIndex), bitIndex), bitIndex + 1)) 0                           
                            |> Seq.take 8
                            |> Seq.toList
                            |> List.rev

    let byteToBitArray b = 
            List.map (fun (bitMask, bitPosition) -> 
                        if (b &&& bitMask) >>> bitPosition = byte(0) then Zero
                        else One) bitMasks

    let bytesToBits (bytes:byte[]) =        
        bytes 
            |> Array.toList
            |> List.map byteToBitArray
            |> List.collect id
            |> List.toArray

    let bitsToUInt (bits:Bit[])  = 
        let positions = Array.zip bits (Array.rev [|0..Array.length bits - 1|])

        Array.fold (fun acc (bit, index) -> 
                        match bit with 
                            | Zero -> acc
                            | One -> acc + (pown 2 index)) 0 positions

    let sizeofType objType = Marshal.SizeOf objType

    let blit<'T>  (byteArray: byte[]) : 'T [] = 
        let size = sizeofType typeof<'T>

        let count = byteArray.Length / size
        let offset = 0 
        let length = byteArray.Length

        let args = (byteArray, count, offset, length)

        let func = OptimizedBlit<'T>.MakeUnsafeArrayBlitParser()

        func.Invoke args


    let byteArrayToObjects<'T> (byteArray: byte[]) networkOrder : 'T [] =         
        blit byteArray |> (if networkOrder then Array.rev else id)


    