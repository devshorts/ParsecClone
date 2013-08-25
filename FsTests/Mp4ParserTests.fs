module Mp4ParserTests

open FsUnit
open NUnit.Framework
open Mp4Matcher
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System.IO

[<Test>]
let moovFtypTest() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream ftyp
    
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
let expectedMatchOnName() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream (mvhd >>. ftyp)
    
    result |> should equal None

[<Test>]
let matchFtypAndMoov() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream video
    
    result.Length |> should equal 3

[<Test>]
let matchOptFtypAndMoov() = 
    
    use f = new FileStream(@"NoFtyp.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream video
    
    result.Length |> should equal 2

[<Test>]
let madeByFfmpeg() = 
    
    use f = new FileStream(@"ffmpegMade.m4v", FileMode.Open)

    let parserStream = new BinStream(f)

    let result = test parserStream video
    
    result.Length |> should equal 5


