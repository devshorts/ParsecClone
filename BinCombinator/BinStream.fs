namespace BinParsers

open System.IO
open Combinator


type BinStream (state:Stream) = 
    interface IStreamP<Stream, byte[]>  with       
        member this.state = state     
        member x.consume (state:IStreamP<Stream, byte[]> ) (count) = 
            let mutable bytes = Array.init count (fun i -> byte(0))
            state.state.Read(bytes, 0, count) |> ignore
            (Some(bytes), new BinStream(state.state) :> IStreamP<Stream, byte[]> )
        
    member x.streamCanBeConsumed (state:IStreamP<Stream, byte[]> ) count =                 
        if (int)state.state.Length >= count then
            Some(count)
        else 
            None
