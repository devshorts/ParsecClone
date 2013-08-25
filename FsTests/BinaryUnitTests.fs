module BinaryUnitTests

open NUnit.Framework
open FsUnit
open System.IO
open Combinator
open BinaryCombinator

let bp = new BinParser(Array.rev)

let byte1 = bp.byte1
let byte2 = bp.byte2
let byte3 = bp.byte3
let byte4 = bp.byte4
let byteN = bp.byteN
let int32 = bp.int32
let int16 = bp.int16
let intB = bp.intB
let uint16 = bp.uint16

[<Test>]
let binTest1() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)

    let parser = byte1

    let result = test parserStream parser 
    
    result |> should equal 0

let byteListToArr arr = 
    arr |> Array.toSeq |> Seq.take 4 |> Seq.toArray |> Array.map byte

[<Test>]
let binTest2() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)

    let parser = manyN 4 byte1 

    let result = test parserStream parser 
    
    result |> should equal (bytes |> byteListToArr)

[<Test>]
let binTest3() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream) 

    let parser = manyN 2 byte4

    let result = test parserStream parser 
    
    result |> should equal [[|0;1;2;3|];[|4;5;6;7|]]

[<Test>]
let binTest4() = 
    let p = new BinParser(id)

    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)   

    let result = test parserStream p.int32 
    
    result |> should equal 50462976


[<Test>]
let binaryWithBacktracker() = 
    let p = new BinParser(id)

    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)   

    let failureParse =  manyN 10 p.int32

    let backtrackWithFail = p.int32 >>=? fun b1 -> 
                            failureParse >>= fun b2 -> 
                            preturn b1

    let consume1 = backtrackWithFail .>>. byte1

    let result = test parserStream consume1
    
    result |> should equal (50462976, byte(4))

[<Test>]
let ``test backtracker with attempt operator``() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)   

    let shouldFail = int32 >>= fun b1 -> 
                               manyN 10 int32 >>= fun b2 -> 
                               preturn b1

    let consume1 = choice[attempt shouldFail; intB]

    let result = test parserStream consume1
    
    result |> should equal 0

[<Test>]
let takeTillTest() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)   

    let takenLower = takeTill (fun i -> i >= byte(4)) byte1
    
    let result = test parserStream takenLower
    
    result |> should equal ([|0;1;2;3|] |> Array.map byte)

[<Test>]
let takeTillTest2() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream)   

    let takeUpper = takeTill (fun i -> i >= byte(4)) byte1 >>. byteN 5    

    let result = test parserStream takeUpper
    
    result |> should equal ([|4;5;6;7;8|] |> Array.map byte)


[<Test>]
let takeWhileTest() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let parserStream = new BinStream(new MemoryStream(bytes))   

    let takeUpper = takeWhile (fun i -> i < byte(4)) byte1 >>. byteN 5    

    let result = test parserStream takeUpper
    
    result |> should equal ([|4;5;6;7;8|] |> Array.map byte)

[<Test>]
let endianessTest() = 
    let bytes = [|0x11;0x7B;0;0;0;0;0;0|] |> Array.map byte

    let parserStream = new BinStream(new MemoryStream(bytes))   

    let result = test parserStream uint16
    
    result |> should equal 4475


[<Test>]
let endianessTest2() = 
    let bytes = [|0xF1;0x7B;0;0;0;0;0;0|] |> Array.map byte

    let parserStream = new BinStream(new MemoryStream(bytes))   

    let result = test parserStream int16
    
    result |> should equal -3717

[<Test>]
let bitParserTest() = 
    let bytes = [|0xF0;0x01|] |> Array.map byte

    let parserStream = new BinStream(new MemoryStream(bytes))   

    let bitToBool = bp.bitsN 4 

    let bitP = bp.makeBitP (byteN 1) bitToBool

    let result = test parserStream (bitP .>> bp.byte1 .>> eof)
    
    result |> should equal 15
