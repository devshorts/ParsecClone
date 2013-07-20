namespace StringCombinator

open StringCombinator.Combinator
open System.Text.RegularExpressions
open System

module StringP = 

    let (|RegexStr|_|) (pattern:string) (input:string) =
        if String.IsNullOrEmpty input then None
        else
            let m = Regex.Match(input, "^" + pattern)
            if m.Success then 
                Some ([ for g in m.Groups -> g.Value ]
                            |> List.filter (String.IsNullOrEmpty >> not)
                            |> List.head) 
            else 
                None

    let private stateChanger (input:string) (target:string) = input.Remove(0, target.Length)

    let private startsWith target (input:string) = if input.StartsWith target then Some target else None

    let private regexMatch target (input:string) = 
        match input with 
            | RegexStr target result -> Some(result)
            | _ -> None
        
    let private stringMatcher = matcher stateChanger

    let matchStr target =                
        stringMatcher startsWith target

    let regexStr pattern =
        stringMatcher regexMatch pattern            
        
    let char = regexStr "[a-z]"

    let chars = regexStr "[a-z]+"

    let digit = regexStr "[0-9]"

    let digits = regexStr "[0-9]+"
   
    let newline = regexStr "\r\n" <|> regexStr "\r" <|> regexStr "\n"

    let whitespace = regexStr "\s"

    let whitespaces = regexStr "\s+"
    
    let space = regexStr " "

    let spaces = regexStr " +"

    let tab = regexStr "\t"

    let tabs = regexStr "\t+"