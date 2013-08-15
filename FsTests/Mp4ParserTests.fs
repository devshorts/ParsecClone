module Mp4ParserTests

open FsUnit
open NUnit.Framework
open Mp4Matcher
open BinaryCombinator
open Combinator
open System.IO

[<Test>]
let moovFtypTest() = 
    
    use f = new FileStream(@"Z:\Data\video\simcap1t_130724_1508_C102_4f383d432da44124bc649682e7c8b825.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream ftypAtom
    
    result |> should equal (FTYP({
                                    MajorBrand = "mp42"           
                                    MinorVersion = (uint32)0x0
                                    Brands = Some([
                                                        "mp42";
                                                        "isom";
                                                        "avc1"
                                                  ])
                                }))



[<Test>]
[<ExpectedException>]
let moovFtypTest2() = 
    
    use f = new FileStream(@"Z:\Data\video\simcap1t_130724_1508_C102_4f383d432da44124bc649682e7c8b825.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream (moov >>. mvhd)
    
    result |> should equal None
