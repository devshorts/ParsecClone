namespace BinaryCombinator 

open Combinator
open System.IO

[<AutoOpen>]
module BinParser = 
    
    let private byteToInt (b:byte) = System.Convert.ToInt32(b)
    let private toInt16 v = System.BitConverter.ToInt16(v, 0)
    let private toInt32 v = System.BitConverter.ToInt32(v, 0)
    let private toInt64 v = System.BitConverter.ToInt64(v, 0)
    
    type ParseState = State<Stream, byte[]>
    
    let private getBinStream (state:ParseState) = (state :?> BinStream)

    let private streamCanBeConsumed (state:ParseState) count  = (state |> getBinStream).streamCanBeConsumed state count
    
    let private binMatch (num:int) = matcher streamCanBeConsumed num        

    let byteN<'a> = binMatch 

    let byte1<'a> = byteN 1 >>= fun b1 -> preturn b1.[0]  

    let byte2<'a> = byteN 2  
    
    let byte3<'a> = byteN 3

    let byte4<'a> = byteN 4

    let int16<'a> = byte2 |>> toInt16
    
    let int32<'a> = byte4 |>> toInt32

    let int64<'a> = byteN 8 |>> toInt64

    let intB<'a> = byte1 |>> byteToInt

