namespace ParsecClone.StringCombinator

open ParsecClone.CombinatorBase
open System.Text.RegularExpressions
open System

[<AutoOpen>]
module StringP = 
    
    type ParseState<'UserState> = IStreamP<string,string,'UserState>

    let foldStrings = fun (strings : string list) -> preturn (List.reduce (+) strings)

    let isMatch regex item = Regex.IsMatch(item, regex)

    let private getStringStream (state:ParseState<'UserState>) = (state :?> StringStreamP<'UserState>)

    let private isEof (input:ParseState<'UserState>) target = not (input.hasMore())

    let private invertRegexMatch (input:ParseState<'UserState>) target = (input |> getStringStream).invertRegexMatch input target 1
    
    let private startsWith (input:ParseState<'UserState>) target = (input |> getStringStream).startsWith input target

    let private regexMatch (input:ParseState<'UserState>) target = (input |> getStringStream).regexMatch input target 

    let regexStr pattern = matcher regexMatch pattern

    let matchStr str = matcher startsWith str

    let invertRegex pattern = matcher invertRegexMatch pattern 
        
    let anyBut = invertRegex

    let char s = regexStr "[a-z]" s

    let chars s = regexStr "[a-z]+" s   

    let digit s = regexStr "[0-9]" s

    let digits s = regexStr "[0-9]+" s
   
    let newline s = (regexStr "\r\n" <|> regexStr "\r" <|> regexStr "\n") s

    let whitespace s = regexStr "\s" s

    let whitespaces s = regexStr "\s+" s
    
    let space s = regexStr " " s

    let spaces s = regexStr " +" s

    let tab s = regexStr "\t" s

    let tabs s = regexStr "\t+" s

    let isDigit = isMatch "[0-9]"

    let isUpper = isMatch "[A-Z]"

    let isLower = isMatch "[a-z]"  

    let any s = regexStr "." s

    let isChar = isMatch "[A-z]"

    let isSpace = function 
                    | " "
                    | "\t" -> true
                    | _ -> false

    let ws s = (opt (many (satisfy isSpace any))
                    >>= function
                        | Some(i) -> preturn (List.reduce (+) i)
                        | None -> preturn "") s

    let isNewLine i = isMatch "\r\n" i || isMatch "\r" i || isMatch "\n" i