namespace StringCombinator

open Combinator
open System
open System.Text.RegularExpressions

type StringStreamP (state:string) = 
    let initialState = state
    let mutable currentState = state

    let (|RegexStr|_|) (pattern:string) (input:IStreamP<string, string>) =
        if String.IsNullOrEmpty input.state then None
        else
            let m = Regex.Match(input.state, "^(" + pattern + ")")
            if m.Success then 
                Some ([ for g in m.Groups -> g.Value ]
                            |> List.filter (String.IsNullOrEmpty >> not)
                            |> List.head) 
            else 
                None


    interface IStreamP<string, string> with
        member x.state with get() = currentState    

        member x.consume count = 
            let result = state.Substring(0, count);
            let newState = state.Remove(0, count)
            (Some(result), new StringStreamP(newState) :> IStreamP<string, string>)

        member x.backtrack () = currentState <- initialState

        member x.hasMore () = not (String.IsNullOrEmpty state)

    member x.startsWith (inputStream:IStreamP<string, string>) target = 
        if String.IsNullOrEmpty inputStream.state then None
        else if inputStream.state.StartsWith target then 
            Some target.Length
        else None

    member x.regexMatch (input:IStreamP<string, string>) target = 
        if String.IsNullOrEmpty input.state then None
        else 
            match input with 
                | RegexStr target result -> Some(result.Length)
                | _ -> None
  
     member x.invertRegexMatch (input:IStreamP<string, string>) target takeAmount = 
        if String.IsNullOrEmpty input.state then None
        else 
            match input with 
                | RegexStr target result -> None
                | _ -> Some(takeAmount)
