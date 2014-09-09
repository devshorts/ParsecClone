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

    let bitMasks = 
        Array.zeroCreate 8        
            |> Array.mapi ( fun i _ -> byte(pown 2 i), i )   
            |> Array.rev

    let private mask inputByte (bitMask, bitPosition) = 
        if   (inputByte &&& bitMask) >>> bitPosition = byte(0) then Zero
        else One 

    let byteToBitArray inputByte  = 
        bitMasks |> Array.map (mask inputByte)                 

    let bytesToBits (bytes:byte[]) =        
        bytes |> Array.collect byteToBitArray

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


    