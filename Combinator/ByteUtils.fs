namespace ParsecClone.BinaryCombinator

open System.Runtime.InteropServices
open System

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

    let inline byteArrToObj<'T> (byteArray : byte[]) : 'T = 
        let handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
        let structure = Marshal.PtrToStructure(handle.AddrOfPinnedObject(), typeof<'T>)
        handle.Free();
        Convert.ChangeType(structure, typeof<'T>) :?> 'T

    let sizeofType objType = Marshal.SizeOf objType

    let inline byteArrayToObjects<'T> (byteArray: byte[]) = 
        let size = sizeofType typeof<'T>
    
        let numObjects = byteArray.Length / size

        let byteRangetoObj count = 
            let start = count * size
            let endS = start + size - 1
            byteArrToObj<'T> (byteArray.[start..endS])

        [0..numObjects - 1]
            |> List.fold(fun acc objNum -> (byteRangetoObj objNum)::acc) []        
