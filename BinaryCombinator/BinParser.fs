namespace BinaryCombinator 

open Combinator
open System.IO

[<AutoOpen>]
module BinParsers = 
    type ParseState = State<Stream, byte[]>
    
    type BinParser (endianNessConverter : byte[] -> byte[]) = 
                
        let getBinStream (state:ParseState) = (state :?> BinStream)

        let streamCanBeConsumed (state:ParseState) count  = (state |> getBinStream).streamCanBeConsumed state count
    
        let streamStartsWith (state:ParseState) bytes  = (state |> getBinStream).streamStartsWith state bytes    

        let binMatch (num:int) = matcher streamCanBeConsumed num    
        
        let binMatchExact bytes = matcher streamStartsWith bytes    

        member x.matchBytes = binMatchExact

        member x.byteToInt (b:byte) = System.Convert.ToInt32(b)
        member x.toInt16 v = System.BitConverter.ToInt16(endianNessConverter v, 0)
        member x.toInt24 v = System.BitConverter.ToInt32(endianNessConverter (Array.append [|byte(0)|] v), 0)
        member x.toInt32 v = System.BitConverter.ToInt32(endianNessConverter v, 0)
        member x.toInt64 v = System.BitConverter.ToInt64(endianNessConverter v, 0)        
    
        member x.byteToUInt (b:byte) = System.Convert.ToUInt32(b)
        member x.toUInt16 v = System.BitConverter.ToUInt16(endianNessConverter v, 0)
        member x.toUInt24 v = System.BitConverter.ToUInt32(endianNessConverter (Array.append [|byte(0)|] v), 0)
        member x.toUInt32 v = System.BitConverter.ToUInt32(endianNessConverter v, 0)
        member x.toUInt64 v = System.BitConverter.ToUInt64(endianNessConverter v, 0)

        member x.byteN = binMatch 

        member x.byte1 = x.byteN 1 >>= fun b1 -> preturn b1.[0]  

        member x.byte2 = x.byteN 2  
    
        member x.byte3 = x.byteN 3

        member x.byte4 = x.byteN 4

        member x.int16 = x.byte2 |>> x.toInt16
    
        member x.int32 = x.byte4 |>> x.toInt32

        member x.int64 = x.byteN 8 |>> x.toInt64

        member x.uint16 = x.byte2 |>> x.toUInt16

        member x.uint32 = x.byte4 |>> x.toUInt32

        member x.uint53 = x.byteN 8 |>> x.toUInt64

        member x.intB = x.byte1 |>> x.byteToInt

        member x.uintB = x.byte1 |>> x.byteToUInt

