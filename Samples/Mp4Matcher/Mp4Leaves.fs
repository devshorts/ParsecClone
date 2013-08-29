namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System

[<AutoOpen>]
module Mp4Leaves = 
   
    let udta : VideoParser<_> = 
        atom "udta"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> UDTA

    let edts : VideoParser<_> = 
        atom "edts"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        freeOpt >>. preturn id |>> EDTS
        
    let uuid : VideoParser<_> = 
        atom "uuid"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        freeOpt >>. preturn id                                      
                           
    let ftyp : VideoParser<_> = 
        atom "ftyp" >>= fun id -> 
        stringId         >>= fun majorBrand ->
        bp.uint32        >>= fun minorVersion ->
            let brands = ((int)id.Size - 16) / 4
            freeOpt >>. opt (manyN brands stringId) >>= fun foundBrands ->
                preturn {
                    MajorBrand = majorBrand;
                    MinorVersion = minorVersion;
                    Brands = foundBrands
                } |>> FTYP

    let mvhd : VideoParser<_> = 
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
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            CreationTime = creationTime
            ModificationTime = modificationTime
            TimeScale = timeScale
        } |>> MVHD


    let iods : VideoParser<_> = 
        atom "iods" >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id |>> IODS

    let tkhd : VideoParser<_> = 
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
        freeOpt >>. preturn {
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
    
    let vmhd : VideoParser<_> = 
        atom "vmhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint16           >>= fun graphicsMode ->
        bp.uint16           >>= fun opcodeRed ->
        bp.uint16           >>= fun opcodeGreen ->
        bp.uint16           >>= fun opcodeBlue ->
        freeOpt >>. 
            (getUserState >>= fun state ->
            setUserState { state with IsAudio = false }) >>. 
                preturn id |>> VMHD

    let smhd : VideoParser<_> = 
        atom "smhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint16           >>= fun balance ->
        bp.skip 2           >>= fun _ ->        
        freeOpt >>. 
            (getUserState >>= fun state ->
            setUserState { state with IsAudio = true })   >>. 
                preturn id |>> SMHD

    let drefEntry : VideoParser<_> = 
        bp.uint32           >>= fun size ->
        bp.byte4 |>> System.Text.Encoding.ASCII.GetString >>= fun ``type`` ->
        versionAndFlags     >>= fun vFlags ->
        freeOpt >>. preturn ()

    let dref : VideoParser<_> = 
        atom "dref"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) drefEntry >>= fun entries ->
        freeOpt >>. preturn id |>> DREF

    let dinf : VideoParser<_> = 
        atom "dinf"    >>= fun id ->
        freeOpt >>. dref |>> DINF

    let mdhd : VideoParser<_> = 
        atom "mdhd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        date                >>= fun creationTime ->
        date                >>= fun modificationTime ->
        bp.uint32           >>= fun timeScale ->
        bp.uint32           >>= fun duration ->
        bp.uint16           >>= fun language ->
        bp.uint16           >>= fun quality ->
        freeOpt >>. preturn id |>> MDHD

    let hdlr : VideoParser<_> = 
        atom "hdlr"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun componentType ->
        bp.uint32           >>= fun componentSubType ->
        bp.uint32           >>= fun componentManufacturer ->
        bp.uint32           >>= fun flags ->
        bp.uint32           >>= fun flagMask ->
        bp.byteN ((int)id.Size - 32) |>> System.Text.Encoding.ASCII.GetString >>= fun componentName ->
        freeOpt >>. preturn id |>> HDLR

