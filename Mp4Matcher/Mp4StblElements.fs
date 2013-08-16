namespace Mp4Matcher

open Combinator
open BinaryCombinator
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
        basicAtom "stts" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) timeToSample >>= fun samples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SampleTimes = samples
        } |>> STTS

   
    let stsd<'a> = 
        basicAtom "stsd"    >>= fun id ->
        skipRemaining id.Size 8 >>= fun _ ->
        preturn id |>> STSD

    let sampleSizeEntry<'a> = bp.uint32 >>= fun i -> preturn { SampleSize = i }

    let stsz<'a> = 
        basicAtom "stsz"    >>= fun id ->
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
        basicAtom "stsc" >>= fun id ->
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
        basicAtom "stco"    >>= fun id ->
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
        basicAtom "stss" >>= fun id ->
        versionAndFlags     >>= fun vFlags ->
        bp.uint32           >>= fun numEntries ->
        manyN ((int)numEntries) sampleNumber >>= fun syncSamples ->
        preturn {
            Atom = id
            VersionAndFlags = vFlags
            NumberOfEntries = numEntries
            SyncSamples = syncSamples
        } |>> STSS
       
