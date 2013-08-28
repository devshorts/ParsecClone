namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System

[<AutoOpen>]
module Mp4StblElements =
    
    let timeToSample = 
        bp.uint32       >>= fun sampleCount ->
        bp.uint32       >>= fun sampleDuration ->
        preturn {
            SampleCount = sampleCount
            SampleDuration = sampleDuration
        }

    let stts : VideoParser<_> = 
        atom "stts" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) timeToSample >>= fun samples ->
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleTimes = samples
        } |>> STTS

    let ctts : VideoParser<_> = 
        atom "ctts"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        freeOpt >>. preturn id  |>> CTTS

    let stsd : VideoParser<_> = 
        atom "stsd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        freeOpt >>. sampleDescription  |>> STSD

    let sampleSizeEntry = bp.uint32 >>= fun i -> preturn { SampleSize = i }

    let stsz : VideoParser<_> = 
        atom "stsz"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun sampleSize ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleSizeEntry >>= fun samples ->
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleSizes = samples
        } |>> STSZ


    let sampleToChunkEntry = 
        bp.uint32 >>= fun firstChunk ->
        bp.uint32 >>= fun samplesPerChunk ->
        bp.uint32 >>= fun sampleDescriptionId ->
        preturn {
            FirstChunk = firstChunk
            SamplesPerChunk = samplesPerChunk
            SampleDescriptionID = sampleDescriptionId
        }

    let stsc : VideoParser<_> = 
        atom "stsc" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleToChunkEntry >>= fun samples ->
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleChunks = samples
        } |>> STSC

    let chunkOffSet = bp.uint32 >>= fun i -> preturn { ChunkOffset = i }

    let stco : VideoParser<_> = 
        atom "stco"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) chunkOffSet >>= fun offsets ->
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            ChunkOffsets = offsets
        } |>> STCO

    let sampleNumber = bp.uint32 >>= fun i -> preturn { SampleNumber = i }

    let stss : VideoParser<_> = 
        atom "stss" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleNumber >>= fun syncSamples ->
        freeOpt >>. preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SyncSamples = syncSamples
        } |>> STSS
       
