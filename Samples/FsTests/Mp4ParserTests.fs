module Mp4ParserTests

open FsUnit
open NUnit.Framework
open Mp4Matcher
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System.IO

let mp4Stream f = new BinStream<VideoState>(f, { IsAudio = false; CurrentStatePosition = (int64)0})

[<Test>]
let moovFtypTest() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

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
let testUnknown() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream unknown 

    result.Name |> should equal "ftyp"

[<Test>]
[<ExpectedException>]
let testUnknownFailure() = 
    
    use f = new MemoryStream([|0x00;0x00;0x00;0x00;0xD0;0xFF;0x01;0x01|] |> Array.map byte)

    let parserStream = mp4Stream f

    let result = test parserStream unknown
    
    result.Name |> should equal "ftyp"

[<Test>]
[<ExpectedException>]
let expectedMatchOnName() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream (mvhd >>. ftyp)
    
    result |> should equal None

[<Test>]
let matchFtypAndMoov() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream video
    
    result.Length |> should equal 3

[<Test>]
let convertToRecordTest1() = 
    
    use f = new FileStream(@"WithFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream video
    
    let record = rootListToRecord result
    
    ()

[<Test>]
let matchOptFtypAndMoov() = 
    
    use f = new FileStream(@"NoFtyp.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream video
    
    result.Length |> should equal 2

[<Test>]
let testMessedUpFreeAtoms() = 
    
    use f = new FileStream(@"z:\data\video\Local_130814_1633_C105_f199e2f2d960428bb448fd2ccd172b7b.m4v", FileMode.Open)

    let parserStream = mp4Stream (new BufferedStream(f))

    let result = test parserStream video
    
    result.Length |> should equal 3

[<Test>]
let sampleFromAppl1() = 
    use f = new FileStream(@"sample_iPod.m4v", FileMode.Open)

    let parserStream = mp4Stream (new BufferedStream(f))

    let result = test parserStream video
    
    // even though it actually has 5 atoms at the root (including 2 free's)
    // the sub atoms have optionally consumed the free's and discarded it. since
    // we don't care about free's this is OK, so the length is actually 1 less

    result.Length |> should equal 4

[<Test>]
let sampleFromAppl2() = 
    use f = new FileStream(@"sample_mpeg4.mp4", FileMode.Open)

    let parserStream = mp4Stream (new BufferedStream(f))

    let result = test parserStream video
    
    // even though it actually has 5 atoms at the root (including 2 free's)
    // the sub atoms have optionally consumed the free's and discarded it. since
    // we don't care about free's this is OK, so the length is actually 1 less

    result.Length |> should equal 4

[<Test>]
let madeByFfmpeg() = 
    
    use f = new FileStream(@"ffmpegMade.m4v", FileMode.Open)

    let parserStream = mp4Stream f

    let result = test parserStream video
    
    printf "%s" <| result.ToString()

    result.Length |> should equal 4


[<Test>]
let findStts() = 
    for file in Directory.EnumerateFiles(@"C:\temp\errs\test2") do 
        printfn "---"
        printfn "%s" file

        use f = new FileStream(file, FileMode.Open)

        use buff = new BufferedStream(f)

        let parserStream = mp4Stream buff

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
[<Ignore>]
let bigVidTest() = 
    
    let now = System.DateTime.Now

    use f = new FileStream(@"c:\temp\bigVid1.m4v", FileMode.Open)

    use buff = new BufferedStream(f)

    let parserStream = mp4Stream buff

    let result = test parserStream video |> rootListToRecord
        
    printfn "took %s" ((System.DateTime.Now - now).ToString())

    () |> should equal ()

