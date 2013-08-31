namespace Mp4Matcher

open System
open ParsecClone.CombinatorBase
open ParsecClone.BinaryCombinator

[<AutoOpen>]
module Mp4DataTypes = 

    type MaybeBuilder () = 
        member x.Bind (result:'a option, func) = 
            if result.IsSome then func(result.Value)
            else None

        member x.Return value = Some(value)
        
        member x.Delay f = f()    
        
        member x.Zero() = None                   

    let maybe = new MaybeBuilder()

    type VideoState = { IsAudio: bool; CurrentStatePosition: int64 }

    type VideoParser<'Return> = Parser<'Return, System.IO.Stream, byte[], VideoState>

    let mp4Stream f = new BinStream<VideoState>(f, { IsAudio = false; CurrentStatePosition = (int64)0})

    let knownAtoms = [  "free";
                        "esds";
                        "avcC";
                        "btrt";
                        "uuid";
                        "colr";
                        "pasp";
                        "stts";
                        "ctts";
                        "stsd";
                        "stsz";
                        "stsc";
                        "stco";
                        "stss";                        
                        "edts";
                        "uuid";
                        "ftyp";
                        "mvhd";
                        "iods";
                        "tkhd";
                        "vmhd";
                        "smhd";
                        "dref";
                        "dinf";
                        "mdhd";
                        "hdlr";
                        "stbl";
                        "minf";
                        "mdia";
                        "trak";
                        "mdat";
                        "moov";]
    type AtomBase = {
        Size: uint32
        Name: string
    }

    type Matrix = uint32 [,]

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
        Matrix: Matrix
        TimeScale : uint32
        PreviewTime : uint32
        PreviewDuration : uint32
        PosterTime : uint32
        SelectionTime : uint32
        CurrentTime : uint32
        NextTrackId : uint32
        SelectionDuration : uint32
    }

    type Tkhd= {
        Atom : AtomBase
        VersionAndFlags: VersionAndFlags
        CreationTime : DateTime
        ModificationTime : DateTime
        TrackId: uint32
        Duration: uint32
        Layer: uint16
        AlternateGroup: uint16
        Volume: uint16
        Height: uint32
        Width: uint32
    }

    type TimeToSampleEntry = { SampleCount: uint32; SampleDuration: uint32 }

    type SampleSizeEntry = { SampleSize : uint32 }

    type SampleToChunkEntry = { FirstChunk: uint32; SamplesPerChunk: uint32; SampleDescriptionID: uint32 }

    type ChunkOffsetEntry = { ChunkOffset: uint32 }

    type SyncSampleEntry = { SampleNumber: uint32 }

    type Stts = {
        Atom: AtomBase
        VersionAndFlags: VersionAndFlags
        NumberOfEntries: uint32
        SampleTimes: TimeToSampleEntry list
    }

    type Stsz = {
        Atom: AtomBase
        VersionAndFlags: VersionAndFlags
        NumberOfEntries: uint32
        SampleSizes: SampleSizeEntry list
    }

    type Stsc = {
        Atom: AtomBase
        VersionAndFlags: VersionAndFlags
        NumberOfEntries: uint32
        SampleChunks: SampleToChunkEntry list
    }

    type Stco = {
        Atom: AtomBase
        VersionAndFlags: VersionAndFlags
        NumberOfEntries: uint32
        ChunkOffsets: ChunkOffsetEntry list
    }

    type Stss = {
        Atom: AtomBase
        VersionAndFlags: VersionAndFlags
        NumberOfEntries: uint32
        SyncSamples: SyncSampleEntry list
    }
    
    (* A tree for use with parsing *)

    type StsdTypes = 
        | STSD_AUDIO of unit
        | STSD_VIDEO of unit
    type StblTypes = 
        | STTS of Stts
        | STSD of StsdTypes
        | STSZ of Stsz
        | STSC of Stsc
        | STCO of Stco
        | STSS of Stss    
        | CTTS of AtomBase 
        | UNKNOWN of AtomBase
    type DinfTypes = 
        | DREF of AtomBase   
        | UNKNOWN of AtomBase
    type MinfTypes = 
        | VMHD of AtomBase
        | SMHD of AtomBase
        | DINF of DinfTypes
        | STBL of StblTypes list
        | UNKNOWN of AtomBase
    type MdiaTypes = 
        | MDHD of AtomBase
        | HDLR of AtomBase
        | MINF of MinfTypes list
        | UNKNOWN of AtomBase
    type TrakTypes = 
        | TKHD of Tkhd
        | EDTS of AtomBase
        | MDIA of MdiaTypes list    
        | UNKNOWN of AtomBase
    type MoovTypes = 
        | MVHD of Mvhd
        | IODS of AtomBase
        | UDTA of AtomBase
        | TRAK of TrakTypes list
        | UNKNOWN of AtomBase
    type Atom = 
        | FTYP of Ftyp 
        | MOOV of MoovTypes list
        | MDAT of AtomBase
        | FREE of AtomBase
        | UNKNOWN of AtomBase
    

    (* Records that represent the transformed tree *)

    type Stbl = {
        Stts: Stts option
        Stsz: Stsz option
        Stsc: Stsc option
        Stco: Stco option
        Stss: Stss option
    }
    type VSMhd = {
        Video: AtomBase option
        Audio: AtomBase option
    }
    type Dref = {
        Dref: AtomBase    
    }
    type Minf = {
        MediaTypeHeader: VSMhd option
        Dinf: Dref option
        Stbl: Stbl option
    }
    type Mdia = {
        Mdhd: AtomBase option
        Hdlr: AtomBase option
        Minf: Minf option
    }
    type Trak = {
        Header: Tkhd option
        Edts: AtomBase option
        Mdia: Mdia option
    }
    type Mov = {
        Mvd: Mvhd option
        Iods: AtomBase option
        Traks: Trak list 
    }
    type Movie = {     
        Mov: Mov option
    }

    (* Functions to transform the tree into records for easier querying *)

    let stblListToRecord stbls = 
        let seed = { Stts = None; Stsz = None; Stsc =  None; Stco = None; Stss = None }
        List.fold (fun acc i ->
            match i with 
            | STTS(x) -> { acc with Stts = Some(x) }
            | STSZ(x) -> { acc with Stsz = Some(x) }
            | STSC(x) -> { acc with Stsc = Some(x) }
            | STCO(x) -> { acc with Stco = Some(x) }
            | STSS(x) -> { acc with Stss = Some(x) }
            | _ -> acc) seed stbls


    let minfListToRecord minfs = 
        let seed = { MediaTypeHeader = None; Dinf = None; Stbl = None }
        List.fold (fun acc i ->
            match i with 
                | VMHD(x) -> { acc with MediaTypeHeader = Some({ Video = Some(x); Audio = None }) }
                | SMHD(x) -> { acc with MediaTypeHeader = Some({ Video = None;    Audio = Some(x) }) }
                | DINF(DREF(x)) -> { acc with Dinf = Some({ Dref = x }) } 
                | STBL(xs) -> { acc with Stbl = Some(stblListToRecord xs) }) seed minfs
                    
    let mdiaListToRecord mdias = 
        let seed = { Mdhd = None; Hdlr = None; Minf = None }
        List.fold (fun acc i ->
            match i with 
                | MINF(xs) -> { acc with Minf = Some(minfListToRecord xs) }
                | HDLR(x) -> { acc with Hdlr = Some(x) }
                | MDHD(x) -> { acc with Mdhd = Some(x) }) seed mdias

    let trackListToRecord tracks = 
        let seed = { Header = None; Edts = None; Mdia = None }
        List.fold (fun acc i -> 
            match i with 
                | TKHD(x) -> { acc with Header = Some(x) }
                | EDTS(x) -> { acc with Edts = Some(x) }
                | MDIA(x) -> { acc with Mdia = Some(mdiaListToRecord x) }
                | _ -> acc) seed tracks

    let moovListToRecord moovs = 
        let seed = { Mvd = None; Iods = None; Traks = [] }
        List.fold (fun acc i ->
            match i with 
                | MVHD(x) -> { acc with Mvd = Some(x) }
                | IODS(x) -> { acc with Iods = Some(x) }
                | TRAK(xs) -> { acc with Traks = (trackListToRecord xs)::acc.Traks }
                | _ -> acc) seed moovs

    let rootListToRecord roots = 
        let seed = { Mov = None }
        List.fold (fun acc i ->
            match i with 
                | MOOV(xs) -> { acc with Mov = Some(moovListToRecord xs) }
                | _ -> acc) seed roots
                                

