module ManyUnitTests

open NUnit.Framework
open FsUnit
open System.IO
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase

let (bp: BinParser<unit>) = new BinParser<_> (id)

let pByteZero = bp.matchBytes [| 00uy |] |>> (fun _ -> 00uy)
let pByteOne = bp.matchBytes [| 01uy |] |>> (fun _ -> 01uy)

let parser = parse {
  let! zeros = many pByteZero
  let! ones = many pByteOne
  return zeros, ones
}

[<Test>]
let testMany () = 
  let bytes = [| 01uy; 02uy |]
  let s = new MemoryStream (bytes) |> makeBinStream
  let result = test s parser
  result |> should equal (([]: byte list), [1uy])