namespace StringCombinator

open Combinator
open StringCombinator
open System.Text.RegularExpressions
open System

[<AutoOpen>]
module StringP = 

    type ParseState = State<string, string>

    let private getStringStream (state:ParseState) = (state :?> StringStreamP)

    let private startsWith (input:ParseState) target = (input |> getStringStream).startsWith input target

    let private regexMatch (input:ParseState) target = (input |> getStringStream).regexMatch input target 
    
    let private invertRegexMatch (input:ParseState) target = (input |> getStringStream).invertRegexMatch input target 1
    
    let matchStr str = matcher startsWith str

    let regexStr pattern = matcher regexMatch pattern

    let invertRegex pattern = matcher invertRegexMatch pattern 
        
    let anyBut<'a> = invertRegex

    let char<'a> = regexStr "[a-z]"

    let chars<'a> = regexStr "[a-z]+"    

    let digit<'a> = regexStr "[0-9]"

    let digits<'a> = regexStr "[0-9]+"
   
    let newline<'a> = regexStr "\r\n" <|> regexStr "\r" <|> regexStr "\n"

    let whitespace<'a> = regexStr "\s"

    let whitespaces<'a> = regexStr "\s+"
    
    let space<'a> = regexStr " "

    let spaces<'a> = regexStr " +"

    let tab<'a> = regexStr "\t"

    let tabs<'a> = regexStr "\t+"
