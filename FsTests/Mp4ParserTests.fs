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


[<Test>]
let findStts() = 
    for file in ["a1"; "b1"; "c1"; "d1"] do 
        printfn "---"
        printfn "%s" file

        use f = new FileStream(@"c:\temp\errs\" + file + ".m4b", FileMode.Open)

        use buff = new BufferedStream(f)

        let parserStream = makeBinStream buff

        let result = test parserStream video |> rootListToRecord   

        let getAudioMetadata src = maybe {
            let extractAudio (trak:Trak) = maybe {
                let! mdia = trak.Mdia
                let! minf = mdia.Minf
                let! header =  minf.MediaTypeHeader
                let! audio = header.Audio
                let! stbl = minf.Stbl
                let! stts = stbl.Stts
                return stts
            }

            let! mov = src.Mov
            let tracks = mov.Traks

            return List.map extractAudio tracks |> List.filter Option.isSome |> List.map Option.get                           
        }

        let audioStts = match getAudioMetadata result with
                            | Some(stts::_) -> stts
                            | _ -> failwith "shouldn't have gotten here"

        let fuckedUp = List.zip audioStts.SampleTimes [0..(int)audioStts.NumberOfEntries - 1]
                            |> List.filter (fun (i, b) -> (int)i.SampleDuration > 2000) 
                            |> List.map (fun (i, b) -> System.String.Format("{0}: {1}", b, i.SampleDuration))

        List.map (printfn "%s") fuckedUp |> ignore

        () |> should equal ()



[<Test>]
let bigVidTest() = 
    
    let now = System.DateTime.Now

    use f = new FileStream(@"c:\temp\bigVid1.m4v", FileMode.Open)

    use buff = new BufferedStream(f)

    let parserStream = makeBinStream buff

    let result = test parserStream video |> rootListToRecord
        
    printfn "took %s" ((System.DateTime.Now - now).ToString())

    () |> should equal ()

