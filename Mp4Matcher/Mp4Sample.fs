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

    let minf<'a> = 
        basicAtom "minf" >>= fun id ->       
        many (vmhd <|> dinf <|> stbl) |>> MINF

    let mdia<'a> = 
        basicAtom "mdia" >>= fun id ->        
        many (mdhd <|> hdlr <|> minf) |>> MDIA

    let trak<'a> = 
        basicAtom "trak" >>= fun id ->        
        many (tkhd <|> mdia) |>> TRAK

    let moov<'a> = basicAtom "moov" >>. many (mvhd <|> iods <|> trak) |>> MOOV

    let optionalFtyp = attempt ftyp >>= fun i -> preturn (Some(i) |> FTYP)

    let video<'a> = many (choice[ optionalFtyp; moov ])
                        