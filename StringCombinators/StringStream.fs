namespace StringCombinators

open Combinator
open System
open System.Text.RegularExpressions

type StringStreamP (state:string) = 

    let (|RegexStr|_|) (pattern:string) (input:IStreamP<string, string>) =
        if String.IsNullOrEmpty input.state then None
        else
            let m = Regex.Match(input.state, "^" + pattern)
            if m.Success then 
                Some ([ for g in m.Groups -> g.Value ]
                            |> List.filter (String.IsNullOrEmpty >> not)
                            |> List.head) 
            else 
                None


    interface IStreamP<string, string> with
        member x.state = state    

        member x.consume inputStream count = 
            let result = inputStream.state.Substring(0, count);
            let newState = inputStream.state.Remove(0, count)
            (Some(result), new StringStreamP(newState) :> IStreamP<string, string>)

        member x.backtrack () = ()

    member x.startsWith (inputStream:IStreamP<string, string>) target = 
        if inputStream.state.StartsWith target then 
            Some target.Length
        else None

    member x.regexMatch (input:IStreamP<string, string>) target = 
        match input with 
            | RegexStr target result -> Some(result.Length)
            | _ -> None
  
