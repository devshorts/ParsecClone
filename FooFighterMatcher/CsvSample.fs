namespace StringMatchers

open Combinator
open StringCombinator

module CsvSample = 
    

    let comma<'a> = matchStr ","

    let csvElement = many (invertRegex ",|\r\n|\n|\r" 1) >>= fun chars -> preturn (List.fold (+) "" chars)

    let csvWithComma<'a> = csvElement .>> comma

    let element<'a> = choice[attempt csvWithComma; csvElement]

    let elements<'a> = many element

    let lines<'a> = many (sepBy elements newline)