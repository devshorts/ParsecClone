namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
        
    let bp = new BinParser(Array.rev)

    type AtomBase = {
        Size: uint32
        Name: string
    }

    type VersionAndFlags = {
        Version: uint32;
        Flags : uint32;
    }

    type Ftyp = { 
        MajorBrand: string; 
        MinorVersion: uint32; 
        Brands: string list option
    }

    type Mvhd = {
        Atom : AtomBase
        VersionAndFlags: VersionAndFlags
        CreationTime : DateTime
        ModificationTime : DateTime
        TimeScale : uint32
    }

    type TrakTypes = 
        | TKHD of AtomBase
        | MDIA of AtomBase
    
    type MoovTypes = 
        | MVHD of Mvhd
        | IODS of AtomBase
        | TRAK of TrakTypes list
    type Atom = 
        | FTYP of Ftyp
        | MOOV of MoovTypes list
                    

    
    let float4<'a> = bp.byte4 |>> Convert.ToDouble

    let private unixEpoch = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date<'a> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> unixEpoch.AddMilliseconds)
          
    let stringId<'a> = bp.byte4  |>> Encoding.ASCII.GetString

    let name<'a> (value:string) = bp.matchBytes (Encoding.ASCII.GetBytes value) |>> Encoding.ASCII.GetString

    let atomSize<'a> = bp.uint32

    let skipRemaining start consumed = bp.byteN ((int)start - consumed)

    let basicAtom<'a> id =
        attempt (
            atomSize >>= fun size ->
            name id >>= fun name ->
                preturn 
                    {
                        Size = size
                        Name = name
                    }
        )

    let versionAndFlags = 
        bp.byte1 |>> bp.byteToUInt  >>= fun version ->
        bp.byte3 |>> bp.toUInt24    >>= fun flags ->
        preturn 
            {
                Version = version
                Flags = flags
            }
                            
    let ftyp<'a> = 
        basicAtom "ftyp" >>= fun id -> 
        stringId         >>= fun majorBrand ->
        bp.uint32        >>= fun minorVersion ->
            let brands = ((int)id.Size - 16) / 4
            opt (manyN brands stringId) >>= fun foundBrands ->
                preturn {
                    MajorBrand = majorBrand;
                    MinorVersion = minorVersion;
                    Brands = foundBrands
                } |>> FTYP

    let mvhd<'a> = 
        basicAtom "mvhd"            >>= fun id -> 
        versionAndFlags             >>= fun vFlags ->
        date                        >>= fun creationTime ->
        date                        >>= fun modificationTime ->
        bp.uint32                   >>= fun timeScale ->
        bp.uint32                   >>= fun duration ->
        bp.uint32                   >>= fun rate ->
        bp.uint16                   >>= fun volume ->
        bp.byteN 70                 >>= fun _ -> 
        bp.uint32                   >>= fun nextTrackId ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            CreationTime = creationTime
            ModificationTime = modificationTime
            TimeScale = timeScale
        } |>> MVHD


    let iods<'a> = 
        basicAtom "iods" >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id |>> IODS

    let tkhd<'a> = 
        basicAtom "tkhd" >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id |>> TKHD

    let mdia<'a> = 
        basicAtom "mdia" >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id |>> MDIA

    let trak<'a> = 
        basicAtom "trak" >>= fun id ->        
        many (tkhd <|> mdia) |>> TRAK

    let moov<'a> = basicAtom "moov" >>. many (mvhd <|> iods <|> trak)