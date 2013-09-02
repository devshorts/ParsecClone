open System.Runtime.InteropServices
open System

let byteArrToObj (byteArray : byte[]) objType = 
    let handle = GCHandle.Alloc(byteArray, GCHandleType.Pinned);
    let structure = Marshal.PtrToStructure(handle.AddrOfPinnedObject(), objType)
    handle.Free();
    Convert.ChangeType(structure, objType)        

let byteArrayToObjArry (byteArray: byte[]) objType = 
    let size = Marshal.SizeOf(objType)
    
    let numObjects = byteArray.Length / size

    let byteRangetoObj count = 
        let start = count * size
        let endS = start + size - 1
        byteArrToObj (byteArray.[start..endS]) objType

    [0..numObjects - 1]
        |> List.fold(fun acc objNum -> (byteRangetoObj objNum)::acc) []