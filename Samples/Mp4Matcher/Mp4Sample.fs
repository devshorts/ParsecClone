namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
    
    let stblInsides = 
                    (stts >>-- injector "stts") <|> 
                    (stsd >>-- injector "stsd")  <|> 
                    (stsz >>-- injector "stsz")  <|> 
                    stsc >>-- injector "stsc" <|> 
                    stco >>-- injector "stco" <|> 
                    stss >>-- injector "stss" <|> 
                    ctts >>-- injector "ctts" <|>
                    (unknown |>> StblTypes.UNKNOWN) >>-- injector "unknown" 

    let stbl = fullConsume "stbl" (fun id ->  stblInsides) >>-- injector "stbl" |>> STBL

    let vOrSmhd = vmhd <|> smhd

    let minf = 
        freeOpt >>.
        fullConsume "minf" 
            (fun id -> 
                vOrSmhd <|> 
                dinf <|> 
                stbl <|>
                (unknown |>> MinfTypes.UNKNOWN)) >>-- injector "minf" |>> MINF

    let mdia = 
        freeOpt >>.
        fullConsume "mdia"
            (fun id ->        
                mdhd <|> 
                hdlr <|> 
                minf <|>
                (unknown |>> MdiaTypes.UNKNOWN)
            ) >>-- injector "mdia" |>> MDIA

    let trak = 
        freeOpt >>.
        fullConsume "trak" 
            (fun id ->        
                tkhd <|> 
                mdia <|> 
                edts <|>
                (unknown |>> TrakTypes.UNKNOWN)
            ) >>-- injector "trak" |>> TRAK

    let mdat = 
        atom "mdat" >>= fun id ->
        if (int)id.Size = 0 then 
            bp.skipToEnd  >>. preturn id |>> MDAT
        else
            bp.skip ((int)id.Size-8) >>= fun _ ->
            preturn id |>> MDAT

    let moov =         
        fullConsume "moov" 
            (fun id ->
                mvhd <|> 
                iods <|> 
                trak <|>
                (unknown |>> MoovTypes.UNKNOWN)
            ) >>-- injector "moov" |>> MOOV   

    let video : VideoParser<_> = many (choice[  attempt ftyp; 
                                                moov; 
                                                mdat; 
                                                free;]) .>> eof
                        