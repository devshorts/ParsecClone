namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System

[<AutoOpen>]
module Mp4StsdElements = 
        
    let esds<'a> = 
        basicAtom "esds"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id 

    let avcC<'a> = 
        basicAtom "avcC"        >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id 

    let btrt<'a> = 
        basicAtom "btrt"        >>= fun id ->    
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id 

    let soundDescription<'a> = 
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
        preturn ()
    
    let videoDescription<'a> = 
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
        preturn ()

    let videoStsd<'a> = 
        attempt(
                    videoDescription      >>= fun vDesc ->
                    many1 (avcC <|> btrt) >>= fun inner ->
                    preturn ()
               ) |>> STSD_VIDEO

    let audioStsd<'a> = 
        attempt(
                    soundDescription >>= fun sDesc ->
                    esds >>= fun esds ->
                    preturn ()
               ) |>> STSD_AUDIO

    let sampleDescription<'a> = audioStsd <|> videoStsd
    