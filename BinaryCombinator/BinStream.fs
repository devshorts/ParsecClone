namespace BinaryCombinator

open System.IO
open Combinator


type BinStream (state:Stream) =   
    
    let startPos = state.Position
    
    interface IStreamP<Stream, byte[]>  with       
        member x.state = state     

        member x.consume (count) = 
            let bytes = x.initBytes count

            state.Read(bytes, 0, count) |> ignore
            
            System.Console.WriteLine(state.Position.ToString())

            (Some(bytes), new BinStream(state) :> IStreamP<Stream, byte[]> )

        member x.skip count = 
            state.Seek((int64)count, SeekOrigin.Current) |> ignore

            (Some(true),  new BinStream(state) :> IStreamP<Stream, byte[]> )

        member x.backtrack () = state.Seek(startPos, SeekOrigin.Begin) |> ignore

        member x.hasMore () = state.Position <> state.Length

        member x.equals istream = istream.state.Position = startPos
            
        
    member x.initBytes size = Array.init size (fun i -> byte(0))

    member x.streamCanBeConsumed (state:IStreamP<Stream, byte[]> ) count =                 
        if (int)state.state.Position + (int)count <= (int)state.state.Length then
            Some(count)
        else 
            None

    member x.seekToEnd() = state.Seek((int64)0, SeekOrigin.End) |> ignore

    member x.streamStartsWith (input:IStreamP<Stream, byte[]> ) bytes =                 
        if (int)input.state.Position + (Array.length bytes) > (int)input.state.Length then
            None
        else 
            let start = input.state.Position

            let count = Array.length bytes

            let b = x.initBytes count

            input.state.Read(b, 0, count) |> ignore

            input.state.Seek(start, SeekOrigin.Begin) |> ignore

            if b = bytes then
                Some(count)
            else 
                None

