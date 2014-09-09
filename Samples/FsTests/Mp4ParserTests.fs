module Mp4ParserTests

open FsUnit
open NUnit.Framework
open Mp4Matcher
open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System.IO
open System.Reflection
open System

let audioStts result = 
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

        match getAudioMetadata result with
            | Some(stts::_) -> Some(stts)
            | _ -> None
        
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
    
    let audioStts = audioStts (result |> rootListToRecord)
    
    (Option.get audioStts).SampleTimes.[0].SampleDuration |> should equal (uint32 1025)


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

    let audioStts = audioStts (result |> rootListToRecord)

    (Option.get audioStts).SampleTimes.[0].SampleDuration |> should equal (uint32 1025)

    (Option.get audioStts).SampleTimes.[1].SampleDuration |> should equal (uint32 661)

[<Test>]
[<Ignore>]
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
[<Ignore>]
let findStts() = 
    let now = DateTime.Now
    let mutable count = 0
    for file in Directory.EnumerateFiles(@"Z:\data\video") do 
        try            
            let xNow = DateTime.Now
            let duration() = (DateTime.Now - xNow).ToString()

            count <- count + 1
            printf "%s" file

            use f = new FileStream(file, FileMode.Open)

            use buff = new BufferedStream(f)

            let parserStream = mp4Stream buff

            let result = test parserStream video |> rootListToRecord   
        
            let atts = audioStts result

            if Option.isNone atts then printfn "..No audio %s" <| duration()
            else 
                let audioStts = Option.get atts
                let fuckedUp = List.zip (Array.toList audioStts.SampleTimes) [0..(int)audioStts.NumberOfEntries - 1]
                                    |> List.filter (fun (i, b) -> (int)i.SampleDuration > 5000) 
                                    |> List.map (fun (i, b) -> System.String.Format("{0}: {1}", b, i.SampleDuration))

                //List.map (printfn "%s") fuckedUp |> ignore

                printfn "...%s %s" (if List.length fuckedUp > 0 then "has abnormal stts" else "OK") <| duration()

            () |> should equal ()


        with
            exn -> printfn "...error"

    printfn "%d files took %s" count ((DateTime.Now - now).ToString())


[<Test>]
[<Ignore>]
let bigVidTest() = 
    
    for i in [0..1] do
        let now = System.DateTime.Now

        use f = new FileStream(@"z:\data\video\2013-06-24--13-25-20--Encounter--R.12-C.0.m4v", FileMode.Open)

        use buff = new BufferedStream(f)

        let parserStream = mp4Stream buff
    
        MiscUtils.time "parsing video" (fun () -> test parserStream video) |> ignore

        () |> should equal ()

