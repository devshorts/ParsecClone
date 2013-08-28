namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
    
    let stbl = 
        atom "stbl" >>= fun id ->        
        freeOpt >>. many (stts <|> stsd <|> stsz <|> stsc <|> stco <|> stss <|> ctts) |>> STBL

    let vOrSmhd = vmhd <|> smhd

    let minf = 
        atom "minf" >>= fun id ->       
        freeOpt >>. many (vOrSmhd <|> dinf <|> stbl) |>> MINF

    let mdia = 
        atom "mdia" >>= fun id ->        
        many (mdhd <|> hdlr <|> minf) |>> MDIA

    let trak = 
        atom "trak" >>= fun id ->        
        many (tkhd <|> mdia <|> edts) |>> TRAK

    let mdat = 
        atom "mdat" >>= fun id ->
        if (int)id.Size = 0 then 
            bp.skipToEnd  >>. preturn id |>> MDAT
        else
            bp.skip ((int)id.Size-8) >>= fun _ ->
            preturn id |>> MDAT

    let moov = atom "moov" >>. many (mvhd <|> iods <|> trak) |>> MOOV   

    let video : VideoParser<_> = manyTill1 (choice[attempt ftyp; moov; mdat; udta; free]) eof
                        