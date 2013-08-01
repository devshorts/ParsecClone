namespace StringCombinators

open Combinator
open System
open System.Text.RegularExpressions

type StringStreamP (state:string) = 

    let (|RegexStr|_|) (pattern:string) (input:IStreamP<string>) =
        if String.IsNullOrEmpty input.state then None
        else
            let m = Regex.Match(input.state, "^" + pattern)
            if m.Success then 
                Some ([ for g in m.Groups -> g.Value ]
                            |> List.filter (String.IsNullOrEmpty >> not)
                            |> List.head) 
            else 
                None


    interface IStreamP<string> with
        member x.state = state

    member x.startsWith (inputStream:IStreamP<string>) target = 
        if inputStream.state.StartsWith target then 
            Some target 
        else None

    member x.regexMatch (input:IStreamP<string>) target = 
        match input with 
            | RegexStr target result -> Some(result)
            | _ -> None
           
    member x.consumer (inputStream:IStreamP<string>) (result:string)  = 
        let newState = inputStream.state.Remove(0, result.Length)
        (Some(result), new StringStreamP(newState) :> IStreamP<string>)
