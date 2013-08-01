namespace StringCombinators

open Combinator.Combinator
open Combinator
open StringCombinators
open System.Text.RegularExpressions
open System

[<AutoOpen>]
module StringP = 

    type ParseState = State<string>

    let private getStringStream (state:ParseState) = (state :?> StringStreamP)

    let private startsWith (input:ParseState) target = (input |> getStringStream).startsWith input target

    let private regexMatch (input:ParseState) target = (input |> getStringStream).regexMatch input target       
           
    let private consumer (input:ParseState) (result:string)  = (input |> getStringStream).consumer input result         

    let matchStr str = matcher startsWith consumer str

    let regexStr pattern = matcher regexMatch consumer pattern
        
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