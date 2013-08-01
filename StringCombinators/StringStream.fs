namespace StringCombinators

open Combinator

type StringStreamP (state:string) = 
    interface IStreamP<string> with
        member this.state = state