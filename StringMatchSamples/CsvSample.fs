namespace StringMatchers

open Combinator
open StringCombinator

module CsvSample = 
    
    let delimType = ","

    let(|DelimMatch|EscapedType|Other|) i = 
        if i = "\\" || i ="\"" then EscapedType
        else if i = delimType then DelimMatch
        else Other

    let delim<'a> = matchStr delimType

    let quote  = matchStr "\""

    let validNormalChars = function
                            | EscapedType                                
                            | DelimMatch -> false
                            | rest -> not (isNewLine rest)

    let inQuotesChars  = function                                 
                            | "\"" -> false
                            | _ -> true

    let unescape = function
                     | "n" -> "\n"
                     | "r" -> "\r"
                     | "t" -> "\t"                     
                     | c   -> c

    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldChars

    let escapedChar<'a> = matchStr "\\" >>. (anyOf matchStr [delimType; "\"";"n";"r";"t"] |>> unescape)
    
    let normal<'a> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldChars

    let literal<'a> = between quote quoteStrings quote

    let csvElement = ws >>. (literal <|> normalAndEscaped)

    let listItem<'a> = delim >>. opt csvElement

    let elements<'a> = csvElement .<?>>. many listItem

    let lines<'a> = many (elements |> sepBy <| newline) .>> eof