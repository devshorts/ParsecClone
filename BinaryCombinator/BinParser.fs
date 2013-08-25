namespace BinaryCombinator 

open Combinator
open System.IO

[<AutoOpen>]
module BinParsers = 
    type ParseState = State<Stream, byte[]>
    type BitState = State<byte[], Bit[]>
    
    type BinParser (endianNessConverter : byte[] -> byte[]) = 
                
        let getBinStream (state:ParseState) = state :?> BinStream
        let getBitStream (state:BitState) = state :?> BitStream

        let streamCanBeConsumed (state : ParseState) count  = state.canConsume count
        let bitStreamCanBeConsumed (state : BitState) count  = state.canConsume count
    
        let streamStartsWith (state:ParseState) bytes  = (state |> getBinStream).streamStartsWith state bytes            

        let binMatch (num:int) = matcher streamCanBeConsumed num    

        let bitMatch num = matcher bitStreamCanBeConsumed num
        
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

        member x.skip num  = skipper streamCanBeConsumed num

        member x.skipToEnd = 
            fun (state:IStreamP<_,_>) ->
                (state |> getBinStream).seekToEnd()

                (Some(true),  new BinStream(state.state) :> IStreamP<Stream, byte[]> )

        member x.byteN = binMatch 
        member x.bitsN = bitMatch

        member x.byte1 = x.byteN 1 >>= fun b1 -> preturn b1.[0]  

        member x.byte2 = x.byteN 2  
    
        member x.byte3 = x.byteN 3

        member x.byte4 = x.byteN 4

        member x.int16 = x.byte2 |>> x.toInt16
    
        member x.int32 = x.byte4 |>> x.toInt32

        member x.int64 = x.byteN 8 |>> x.toInt64

        member x.uint16 = x.byte2 |>> x.toUInt16

        member x.uint24 = x.byteN 3 |>> x.toInt24

        member x.uint32 = x.byte4 |>> x.toUInt32

        member x.uint64 = x.byteN 8 |>> x.toUInt64

        member x.intB = x.byte1 |>> x.byteToInt

        member x.uintB = x.byte1 |>> x.byteToUInt

        member x.shiftL n = fun (b : uint32)  -> preturn (b <<< n)

        member x.shiftR n = fun (b : uint32) -> preturn (b >>> n)

        member x.floatP = x.byteN 4 >>= fun b -> 
                          preturn (System.BitConverter.ToSingle(endianNessConverter b, 0))

        (* Bit parsing *)

        member x.bitsToInt = bitsToUInt                       

        member x.makeBitP seed parser = reproc (fun (b:byte[]) -> new BitStream(b, 0) :> IStreamP<byte[], Bit[]>) seed parser

        member x.bitN n = 
                          let zeroBasedN = n - 1
                          x.bitsN n >>= fun value -> 
                          preturn value.[zeroBasedN]
                          
        member x.bit1 = x.bitN 1
        member x.bit2 = x.bitN 2
        member x.bit3 = x.bitN 3
        member x.bit4 = x.bitN 4
        member x.bit5 = x.bitN 5
        member x.bit6 = x.bitN 6
        member x.bit7 = x.bitN 7
        member x.bit8 = x.bitN 8

       