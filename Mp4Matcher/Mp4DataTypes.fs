namespace Mp4Matcher

open System

[<AutoOpen>]
module Mp4DataTypes = 

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

    type Tkhd= {
        Atom : AtomBase
        VersionAndFlags: VersionAndFlags
        CreationTime : DateTime
        ModificationTime : DateTime
        TrackId: uint32
        Duration: uint32
        Layer: uint32
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
    type MinfTypes = 
        | VMHD of AtomBase
        | SMHD of AtomBase
        | DINF of DinfTypes
        | STBL of StblTypes list
    type MdiaTypes = 
        | MDHD of AtomBase
        | HDLR of AtomBase
        | MINF of MinfTypes list
    type TrakTypes = 
        | TKHD of Tkhd
        | EDTS of AtomBase
        | MDIA of MdiaTypes list    
        | UNKNOWN of AtomBase
    type MoovTypes = 
        | MVHD of Mvhd
        | IODS of AtomBase
        | TRAK of TrakTypes list
        | UNKNOWN of AtomBase
    type Atom = 
        | FTYP of Ftyp 
        | MOOV of MoovTypes list
        | MDAT of AtomBase
        | UDTA of AtomBase
        | FREE of AtomBase
        | UNKNOWN of AtomBase
                    

