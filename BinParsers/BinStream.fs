namespace BinParsers

open System.IO
open Combinator


type BinStream (state:Stream) = 
    interface IStreamP<Stream> with       
        member this.state = state
        
    member x.streamCanBeConsumed (state:IStreamP<Stream>) count =                 
        if (int)state.state.Length >= count then
            Some((int)count)
        else 
            None

    member x.consumeStream (state:IStreamP<Stream>) (count) =                         
        let mutable bytes = Array.init count (fun i -> byte(0))
        state.state.Read(bytes, 0, count) |> ignore
        (Some(bytes), new BinStream(state.state) :> IStreamP<Stream>)