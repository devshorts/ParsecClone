namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
    
    let stbl<'a> = 
        basicAtom "stbl" >>= fun id ->        
        many (stts <|> stsd <|> stsz <|> stsc <|> stco <|> stss) |>> STBL

    let vOrSmhd<'a> = vmhd <|> smhd

    let minf<'a> = 
        basicAtom "minf" >>= fun id ->       
        many (vOrSmhd <|> dinf <|> stbl) |>> MINF

    let mdia<'a> = 
        basicAtom "mdia" >>= fun id ->        
        many (mdhd <|> hdlr <|> minf) |>> MDIA

    let trak<'a> = 
        basicAtom "trak" >>= fun id ->        
        many (tkhd <|> mdia) |>> TRAK

    let mdat<'a> = 
        basicAtom "mdat" >>= fun id ->
        if (int)id.Size = 0 then 
            bp.skipToEnd  >>. preturn id |>> MDAT
        else
            bp.skip ((int)id.Size-8) >>= fun _ ->
            preturn id |>> MDAT

    let moov<'a> = basicAtom "moov" >>. many (mvhd <|> iods <|> trak) |>> MOOV
    
    let video<'a> = many (choice[attempt ftyp; moov; mdat]) .>> eof
                        