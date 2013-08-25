namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System

[<AutoOpen>]
module Mp4StblElements =
    
    let timeToSample<'a> = 
        bp.uint32       >>= fun sampleCount ->
        bp.uint32       >>= fun sampleDuration ->
        preturn {
            SampleCount = sampleCount
            SampleDuration = sampleDuration
        }

    let stts<'a> = 
        atom "stts" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) timeToSample >>= fun samples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleTimes = samples
        } |>> STTS

   
   
    let ctts<'a> = 
        atom "ctts"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> CTTS

    let stsd<'a> = 
        atom "stsd"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        sampleDescription  |>> STSD

    let sampleSizeEntry<'a> = bp.uint32 >>= fun i -> preturn { SampleSize = i }

    let stsz<'a> = 
        atom "stsz"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun sampleSize ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleSizeEntry >>= fun samples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleSizes = samples
        } |>> STSZ


    let sampleToChunkEntry<'a> = 
        bp.uint32 >>= fun firstChunk ->
        bp.uint32 >>= fun samplesPerChunk ->
        bp.uint32 >>= fun sampleDescriptionId ->
        preturn {
            FirstChunk = firstChunk
            SamplesPerChunk = samplesPerChunk
            SampleDescriptionID = sampleDescriptionId
        }

    let stsc<'a> = 
        atom "stsc" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleToChunkEntry >>= fun samples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleChunks = samples
        } |>> STSC

    let chunkOffSet<'a> = bp.uint32 >>= fun i -> preturn { ChunkOffset = i }

    let stco<'a> = 
        atom "stco"    >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) chunkOffSet >>= fun offsets ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            ChunkOffsets = offsets
        } |>> STCO

    let sampleNumber<'a> = bp.uint32 >>= fun i -> preturn { SampleNumber = i }

    let stss<'a> = 
        atom "stss" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleNumber >>= fun syncSamples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SyncSamples = syncSamples
        } |>> STSS
       
