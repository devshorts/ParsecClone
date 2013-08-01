namespace BinaryCombinator

open System.IO
open Combinator


type BinStream (state:Stream) =     
    let startPos = state.Position
    do
        System.Console.WriteLine("Position is at: " + startPos.ToString()) |> ignore

    interface IStreamP<Stream, byte[]>  with       
        member x.state = state     
        member x.consume (state:IStreamP<Stream, byte[]> ) (count) = 
            let mutable bytes = Array.init count (fun i -> byte(0))
            state.state.Read(bytes, 0, count) |> ignore
            
            (Some(bytes), new BinStream(state.state) :> IStreamP<Stream, byte[]> )

        member x.backtrack () = state.Seek(startPos, SeekOrigin.Begin) |> ignore

        member x.hasMore () = state.Position <> state.Length
            
        
    member x.streamCanBeConsumed (state:IStreamP<Stream, byte[]> ) count =                 
        if (int)state.state.Position + (int)count <= (int)state.state.Length then
            Some(count)
        else 
            None
