module BinaryUnitTests

open NUnit.Framework
open FsUnit
open System.IO
open Combinator
open Combinator.Combinator
open BinParsers.BinParser
open BinParsers

[<Test>]
let binTest1() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream) :> IStreamP<Stream>

    let parser = byte1

    let result = test parserStream parser 
    
    result |> should equal 0

let byteListToArr arr = 
    arr |> Array.toSeq |> Seq.take 4 |> Seq.toArray |> Array.map byte

[<Test>]
let binTest2() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream) :> IStreamP<Stream>

    let parser = manyN 4 byte1 

    let result = test parserStream parser 
    
    result |> should equal (bytes |> byteListToArr)

[<Test>]
let binTest3() = 
    let bytes = [|0;1;2;3;4;5;6;7;8|] |> Array.map byte

    let stream = new MemoryStream(bytes)   

    let parserStream = new BinStream(stream) :> IStreamP<Stream>

    let parser = manyN 2 byte4

    let result = test parserStream parser 
    
    result |> should equal [[|0;1;2;3|];[|4;5;6;7|]]


