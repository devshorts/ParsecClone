namespace StringMatchers

open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

module CsvSample = 
    
    type CsvParser<'Return> = Parser<'Return, string, string, unit>

    let delimType = ","

    let(|DelimMatch|EscapedType|Other|) i = 
        if i = "\\" || i ="\"" then EscapedType
        else if i = delimType then DelimMatch
        else Other

    let delim : CsvParser<_> = matchStr delimType

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

    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldStrings

    let escapedChar : CsvParser<_> = matchStr "\\" >>. (anyOf matchStr [delimType; "\"";"n";"r";"t"] |>> unescape)
    
    let normal : CsvParser<_> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldStrings
    
    let literal: CsvParser<_> = quoteStrings |> between2 quote

    let csvElement: CsvParser<_> = many (literal <|> normalAndEscaped) >>= foldStrings

    let listItem: CsvParser<_> = delim >>. ws >>. opt csvElement

    let elements: CsvParser<_> = csvElement .<?>>. many listItem

    let lines: CsvParser<_> = many (elements |> sepBy <| newline) .>> eof