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
        
    let delim = matchStr delimType

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

    let escapedChar = matchStr "\\" >>. (anyOf matchStr [delimType; "\"";"n";"r";"t"] |>> unescape)
    
    let normal = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldStrings
    
    let literal = quoteStrings |> between2 quote

    let csvElement = many (literal <|> normalAndEscaped) >>= foldStrings

    let listItem = delim >>. ws >>. opt csvElement

    let elements = csvElement .<?>>. many listItem

    let lines: CsvParser<_> = many (elements |> sepBy <| newline) .>> eof