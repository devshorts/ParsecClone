namespace StringMatchers

open Combinator
open StringCombinator

module CsvSample = 
    
    let comma<'a> = matchStr ","

    let quote  = matchStr "\""

    let validNormalChars c = match c with
                                | "\\"
                                | "\""                                
                                | "," -> false
                                | rest -> not (isNewLine rest)

    let inQuotesChars c = match c with                                 
                            | "\"" -> false
                            | _ -> true


    let unescape c = match c with
                     | "n" -> "\n"
                     | "r" -> "\r"
                     | "t" -> "\t"                     
                     | c   -> c


    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldChars

    let escapedChar<'a> = matchStr "\\" >>. (anyOf matchStr [","; "\"";"n";"r";"t"] |>> unescape)
    
    let normal<'a> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldChars

    let literal<'a> = between quote quoteStrings quote

    let csvElement = ws >>. (literal <|> normalAndEscaped)

    let element<'a> = choice[opt csvElement .>> comma ; csvElement |>> Some]

    let elements<'a> = many element

    let lines<'a> = many (elements |> sepBy <| newline)