open System.Runtime.InteropServices
open System

let byteArrToObj<'T> (byteArray : byte[]) : 'T = 
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

[<Struct>]      
type SampleToChunkEntry = 
    struct                    
        val SampleDescriptionID: uint32           
        val SamplesPerChunk: uint32                     
        val FirstChunk: uint32
    end

let b = [1;0;0;0;80;0;0;0;220;16;0;0;1;0;0;0;100;0;0;0;1;0;0;0] |> List.map byte |> List.toArray

let x = byteArrayToObjects<SampleToChunkEntry> b