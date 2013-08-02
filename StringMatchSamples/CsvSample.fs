namespace StringMatchers

open Combinator
open StringCombinator

module CsvSample = 
    
    let foldChars = fun chars -> preturn (List.fold (+) "" chars)

    let comma<'a> = matchStr ","

    let quote  = matchStr "\""

    let validNormalChars = function
                                | "\\"
                                | "\""
                                | "," -> false
                                | rest -> not (isNewLine rest)

    let inQuotesChars = function                                
                                | "\"" -> false
                                | _ -> true


    let unescape c = match c with
                     | "n" -> "\n"
                     | "r" -> "\r"
                     | "t" -> "\t"                     
                     | c   -> c


    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldChars

    let escapedChar<'a> = matchStr "\\" >>. (anyOf matchStr [","] |>> unescape)
    
    let normal<'a> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldChars

    let literal<'a> = between quote quoteStrings quote

    let csvElement = literal <|> normalAndEscaped

    let element<'a> = csvElement |> sepBy <| comma

    let elements<'a> = many element

    let lines<'a> = many (elements |> sepBy <| newline)