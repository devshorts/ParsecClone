namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System

[<AutoOpen>]
module Mp4Leaves = 
   
    let udta<'a> = 
        atom "udta"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> UDTA

    let free<'a> = 
        atom "free"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> FREE
        
   
    let edts<'a> = 
        atom "edts"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id |>> EDTS
        
    let uuid<'a> = 
        atom "uuid"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id                                      
                           
    let ftyp<'a> = 
        atom "ftyp" >>= fun id -> 
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
        atom "mvhd"            >>= fun id -> 
        versionAndFlags             >>= fun vFlags ->
        date                        >>= fun creationTime ->
        date                        >>= fun modificationTime ->
        bp.uint32                   >>= fun timeScale ->
        bp.uint32                   >>= fun duration ->
        bp.uint32                   >>= fun rate ->
        bp.uint16                   >>= fun volume ->
        bp.skip 70                  >>= fun _ -> 
        bp.uint32                   >>= fun nextTrackId ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            CreationTime = creationTime
            ModificationTime = modificationTime
            TimeScale = timeScale
        } |>> MVHD


    let iods<'a> = 
        atom "iods" >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id |>> IODS

    let tkhd<'a> = 
        atom "tkhd" >>= fun id ->
        versionAndFlags >>= fun vFlags ->
        date >>= fun creationTime ->
        date >>= fun modificationTime ->
        bp.uint32 >>= fun trackId ->
        bp.uint32 >>= fun reserved ->
        bp.uint32 >>= fun duration ->        
        bp.uint32 >>= fun layer ->
        bp.uint16 >>= fun alteranteGroup ->
        bp.uint16 >>= fun volume ->
        bp.byteN 8 >>= fun reserved ->
        manyN 9 bp.floatP >>= fun matrix ->
        bp.uint32 >>.. bp.shiftR 16 >>= fun width ->
        bp.uint32  >>.. bp.shiftR 16 >>= fun height ->
        preturn {
            Atom  = id
            VersionAndFlags = vFlags
            CreationTime  = creationTime
            ModificationTime  = modificationTime
            TrackId = trackId
            Duration = duration
            Layer = layer
            AlternateGroup = alteranteGroup
            Volume = volume
            Height = width
            Width = height
        } |>> TKHD
    
    let vmhd<'a> = 
        atom "vmhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint16           >>= fun graphicsMode ->
        bp.uint16           >>= fun opcodeRed ->
        bp.uint16           >>= fun opcodeGreen ->
        bp.uint16           >>= fun opcodeBlue ->
        preturn id |>> VMHD

    let smhd<'a> = 
        atom "smhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint16           >>= fun balance ->
        bp.skip 2           >>= fun _ ->
        preturn id |>> SMHD

    let drefEntry<'a> = 
        bp.uint32           >>= fun size ->
        bp.byte4 |>> System.Text.Encoding.ASCII.GetString >>= fun ``type`` ->
        versionAndFlags     >>= fun vFlags ->
        preturn ()

    let dref<'a> = 
        atom "dref"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) drefEntry >>= fun entries ->
        preturn id |>> DREF

    let dinf<'a> = 
        atom "dinf"    >>= fun id ->
        dref |>> DINF

    let mdhd<'a> = 
        atom "mdhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        date                >>= fun creationTime ->
        date                >>= fun modificationTime ->
        bp.uint32           >>= fun timeScale ->
        bp.uint32           >>= fun duration ->
        bp.uint16           >>= fun language ->
        bp.uint16           >>= fun quality ->
        preturn id |>> MDHD

    let hdlr<'a> = 
        atom "hdlr"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun componentType ->
        bp.uint32           >>= fun componentSubType ->
        bp.uint32           >>= fun componentManufacturer ->
        bp.uint32           >>= fun flags ->
        bp.uint32           >>= fun flagMask ->
        bp.byteN ((int)id.Size - 32) |>> System.Text.Encoding.ASCII.GetString >>= fun componentName ->
        preturn id |>> HDLR

