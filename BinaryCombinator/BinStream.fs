namespace BinaryCombinator

open System.IO
open Combinator


type BinStream (state:Stream) =     
    let startPos = state.Position
    do
        System.Console.WriteLine("Position is at: " + startPos.ToString()) |> ignore

    interface IStreamP<Stream, byte[]>  with       
        member x.state = state     
        member x.consume (count) = 
            let mutable bytes = Array.init count (fun i -> byte(0))
            state.Read(bytes, 0, count) |> ignore
            
            (Some(bytes), new BinStream(state) :> IStreamP<Stream, byte[]> )

        member x.backtrack () = state.Seek(startPos, SeekOrigin.Begin) |> ignore

        member x.hasMore () = state.Position <> state.Length

        member x.equals istream = istream.state.Position = startPos
            
        
    member x.streamCanBeConsumed (state:IStreamP<Stream, byte[]> ) count =                 
        if (int)state.state.Position + (int)count <= (int)state.state.Length then
            Some(count)
        else 
            None
