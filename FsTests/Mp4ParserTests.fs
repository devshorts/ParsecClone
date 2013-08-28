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

    let parserStream = makeBinStream f

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

    let parserStream = makeBinStream f

    let result = test parserStream (mvhd >>. ftyp)
    
    result |> should equal None

[<Test>]
let matchFtypAndMoov() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = makeBinStream f

    let result = test parserStream video
    
    result.Length |> should equal 3

[<Test>]
let convertToRecordTest1() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = makeBinStream f

    let result = test parserStream video
    
    let record = rootListToRecord result

    ()

[<Test>]
let matchOptFtypAndMoov() = 
    
    use f = new FileStream(@"NoFtyp.m4v", FileMode.Open)

    let parserStream = makeBinStream f

    let result = test parserStream video
    
    result.Length |> should equal 2

[<Test>]
let madeByFfmpeg() = 
    
    use f = new FileStream(@"ffmpegMade.m4v", FileMode.Open)

    let parserStream = makeBinStream f

    let result = test parserStream video
    
    result.Length |> should equal 5

(*
[<Test>]
let findStts() = 
    for file in ["a1"; "b1"; "c1"; "d1"] do 
        printfn "---"
        printfn "%s" file

        use f = new FileStream(@"c:\temp\errs\" + file + ".m4b", FileMode.Open)

        use buff = new BufferedStream(f)

        let parserStream = makeBinStream buff

        let result = test parserStream video
    
        let moov = List.find(function | MOOV(x) -> true | _ -> false) result

        let tracks = 
            match moov with
                | MOOV(l) -> 
                    List.filter (function | TRAK(_) -> true | _ -> false) l
                                    |> List.map (function | TRAK(l) -> l)   
                                    |> List.collect id                             
                                    |> List.filter (function | MDIA(x) -> true | _ -> false)
                                    |> List.map (function |MDIA(x) -> x)
                                    |> List.collect id
                                    |> List.filter (function | MINF(x) -> true | _ -> false)
                                    |> List.map (function | MINF(x) -> x)
                                    |> List.collect id
                                    |> List.filter (function | STBL(x) -> true | _ -> false)
                                    |> List.map (function | STBL(x) -> x)
                                    |> List.collect id
                                    |> List.filter(function | STTS(x) -> true | _ -> false)
                                    |> List.map(function | STTS(x) -> x)

                | _ -> failwith "error"

        let (_::audioStts::_) = tracks

        let fuckedUp = List.filter (fun (i, b) -> (int)i.SampleDuration > 2000) (List.zip audioStts.SampleTimes [0..(int)audioStts.NumberOfEntries - 1])
                            |> List.map (fun (i, b) -> System.String.Format("{0}: {1}", b, i.SampleDuration))

        List.map (printfn "%s") fuckedUp |> ignore

        () |> should equal ()

[<Test>]
let bigVidTest() = 
    
    let now = System.DateTime.Now

    use f = new FileStream(@"c:\temp\bigVid1.m4v", FileMode.Open)

    use buff = new BufferedStream(f)

    let parserStream = makeBinStream buff

    let result = test parserStream video
        
    printfn "took %s" ((System.DateTime.Now - now).ToString())

    () |> should equal ()

*)