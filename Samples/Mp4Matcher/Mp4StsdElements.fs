namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System

[<AutoOpen>]
module Mp4StsdElements = 
        
    let esds : VideoParser<_> = 
        atom "esds"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        freeOpt >>. preturn id 

    let avcC : VideoParser<_> = 
        atom "avcC"        >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id 

    let btrt : VideoParser<_> = 
        atom "btrt"        >>= fun id ->    
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id 

    let uuid : VideoParser<_> = 
        atom "uuid"        >>= fun id ->    
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id 

    let colr : VideoParser<_> = 
        atom "colr"        >>= fun id ->    
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id 

    let pasp : VideoParser<_> = 
        atom "pasp"        >>= fun id ->    
        skipRemaining id.Size 8 >>= fun _ ->
        freeOpt >>. preturn id 

    let soundDescription : VideoParser<_> = 
        bp.uint32   >>= fun size ->
        stringId    >>= fun dataFormat ->
        bp.skip 6   >>= fun _ ->
        bp.uint16   >>= fun dRefIndex ->
        bp.uint16   >>= fun version ->
        bp.uint16   >>= fun revisionLevel ->
        bp.uint32   >>= fun vendor ->
        bp.uint16   >>= fun numChannels ->
        bp.uint16   >>= fun sampleSize ->
        bp.uint16   >>= fun compressionId ->
        bp.uint16   >>= fun packetSize ->
        bp.uint32   >>= fun sampleRate ->
        freeOpt >>. preturn ()
    
    let videoDescription : VideoParser<_> = 
        bp.uint32   >>= fun size ->
        stringId    >>= fun dataFormat ->
        bp.skip 6   >>= fun _ ->
        bp.uint16   >>= fun dRefIndex ->
        bp.uint16   >>= fun version ->
        bp.uint16   >>= fun revisionLevel ->
        bp.uint32   >>= fun vendor ->
        bp.uint32   >>= fun temporalQuality ->
        bp.uint32   >>= fun spatialQuality ->
        bp.uint16   >>= fun width ->
        bp.uint16   >>= fun height ->
        bp.uint32   >>= fun horizontalResolution ->
        bp.uint32   >>= fun verticalResolution ->
        bp.uint32   >>= fun dataSize ->
        bp.uint16   >>= fun frameCount ->
        stringId    >>= fun compressorName ->
        bp.uint16   >>= fun colorDepth ->
        bp.uint16   >>= fun colorTableId ->
        bp.skip 28  >>= fun _ ->        
        let x = dataFormat
        freeOpt >>. preturn ()

    let videoStsd : VideoParser<_> = 
        getUserState >>= fun isAudio ->
        if isAudio then 
            pzero 
        else        
            videoDescription      >>= fun vDesc ->
            many1 (avcC <|> btrt <|> pasp <|> colr <|> uuid) >>= fun inner ->
            freeOpt >>. preturn () |>> STSD_VIDEO

    let audioStsd : VideoParser<_> = 
        getUserState >>= fun isAudio ->
        if isAudio then 
            soundDescription >>= fun sDesc ->
            esds >>= fun esds ->
            freeOpt >>. preturn () |>> STSD_AUDIO
        else 
            pzero

    let sampleDescription : VideoParser<_> = audioStsd <|> videoStsd
    