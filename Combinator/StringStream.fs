namespace ParsecClone.StringCombinator

open ParsecClone.CombinatorBase
open System
open System.Text.RegularExpressions

[<AutoOpen>]
module StringStreamP = 
    type StringStreamP<'UserState> (state:string, userState:'UserState) =  
        let mutable userState = userState        
           
        let currentState = state

        do
            if Combinator.enableDebug then
                printfn "current state %s" state

        let (|RegexStr|_|) (pattern:string) (input:IStreamP<string, string, 'UserState>) =
            if String.IsNullOrEmpty input.state then None
            else
                let m = Regex.Match(input.state, "^(" + pattern + ")", RegexOptions.Singleline)
                if m.Success then 
                    Some ([ for g in m.Groups -> g.Value ]
                                |> List.filter (String.IsNullOrEmpty >> not)
                                |> List.head) 
                else 
                    None
    
        interface IStreamP<string, string, 'UserState> with
            member x.state with get() = currentState    

            member x.consume count = 
                
                if Combinator.enableDebug then
                    printfn "reading %d from current state: %s" count state

                let result = state.Substring(0, count);
                let newState = state.Remove(0, count)
                (Some(result), new StringStreamP<'UserState>(newState, userState) :> IStreamP<string, string, 'UserState>)

            member x.skip count = 
                let (r, s) = (x:>IStreamP<_,_,_>).consume count

                (Some(true), s)

            member x.backtrack () =                 
                if Combinator.enableDebug then
                    printfn "backtracking"

                ()

            member x.hasMore () = not (String.IsNullOrEmpty state)

            member x.equals istream = istream.state = state

            member x.canConsume count = if state.Length <= count then Some(count) else None

            member x.getUserState () =  userState

            member x.setUserState s = userState <- s

            member x.position () = (int64)0   

        member x.startsWith (inputStream:IStreamP<string, string, 'UserState>) target = 
            if String.IsNullOrEmpty inputStream.state then None
            else if inputStream.state.StartsWith target then 
                Some target.Length
            else None

        member x.regexMatch (input:IStreamP<string, string, 'UserState>) target = 
            if String.IsNullOrEmpty input.state then None
            else 
                match input with 
                    | RegexStr target result -> Some(result.Length)
                    | _ -> None
  
         member x.invertRegexMatch (input:IStreamP<string, string, 'UserState>) target takeAmount = 
            if String.IsNullOrEmpty input.state then None
            else 
                match input with 
                    | RegexStr target result -> None
                    | _ -> Some(takeAmount)

    let makeStringStream str = new StringStreamP<unit>(str, ())

    let toInterface streamp = streamp :> IStreamP<string, string, unit>