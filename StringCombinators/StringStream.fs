namespace StringCombinators

open Combinator

type StringStreamP (state:string) = 
    interface IStreamP<string> with
        member this.consume target = 
            let newState = state.Remove(0, target.Length)
            new StringStreamP(newState) :> IStreamP<string>

        member this.state = state