namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
        
    let bp = new BinParser(Array.rev)

    type Ftyp = { 
        MajorBrand: string; 
        MinorVersion: uint32; 
        Brands: string list option
    }

    type Mvhd = {
        Version: uint32;
        Flags : uint32;
        CreationTime : DateTime
        ModificationTime : DateTime
        TimeScale : uint32
    }
    
    type Atom = 
        | FTYP of Ftyp
        | MOOV of unit
        | MVHD of Mvhd

    
    let float4<'a> = bp.byte4 |>> Convert.ToDouble

    let private unixEpoch = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date<'a> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> unixEpoch.AddMilliseconds)
          
    let stringId<'a> = bp.byte4  |>> Encoding.ASCII.GetString

    let name<'a> (value:string) = bp.matchBytes (Encoding.ASCII.GetBytes value)

    let atomSize<'a> = bp.uint32

    let ftypAtom<'a> = 
        atomSize >>= fun size ->
        name "ftyp" >>= fun name ->
        stringId >>= fun majorBrand ->
        bp.uint32   >>= fun minorVersion ->
            let brands = ((int)size - 16) / 4
            opt (manyN brands stringId) >>= fun foundBrands ->
                preturn {
                    MajorBrand = majorBrand;
                    MinorVersion = minorVersion;
                    Brands = foundBrands
                } |>> FTYP


    let moov<'a> =
        atomSize >>= fun size ->
        name "moov" >>= fun name ->
            preturn () |>> MOOV

    let mvhd<'a> = 
        atomSize >>= fun size ->
        name "mvhd" >>= fun id ->
        bp.byte1 |>> bp.byteToUInt >>= fun version ->
        bp.byte3 |>> bp.toUInt24 >>= fun flags ->
        date >>= fun creationTime ->
        date >>= fun modificationTime ->
        bp.uint32 >>= fun timeScale ->
        preturn {
            Version = version
            Flags = flags
            CreationTime = creationTime
            ModificationTime = modificationTime
            TimeScale = timeScale
        } |>> MVHD

