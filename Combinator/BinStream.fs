namespace ParsecClone.BinaryCombinator 

open System.IO
open System.Reflection
open System
open ParsecClone.CombinatorBase

[<AutoOpen>]
module BinStreams =     

    type BinStream<'UserState> (state:Stream, userState:'UserState) =   

        do
            bootstrap_combinator()

        let mutable userState = userState
        
        let startPos = state.Position
    
        interface IStreamP<Stream, byte[],'UserState>  with       
            member x.state = state     

            member x.consume (count) = 
                let bytes = x.initBytes count

                state.Read(bytes, 0, count) |> ignore                           

                (Some(bytes), new BinStream<'UserState>(state, userState) :> IStreamP<Stream, byte[], 'UserState> )

            member x.skip count = 
                state.Seek((int64)count, SeekOrigin.Current) |> ignore

                (Some(true),  new BinStream<'UserState>(state, userState) :> IStreamP<Stream, byte[], 'UserState> )

            member x.backtrack () = state.Seek(startPos, SeekOrigin.Begin) |> ignore

            member x.hasMore () = state.Position <> state.Length

            member x.equals istream = istream.state.Position = startPos

            member x.canConsume count = 
                if (int)state.Position + (int)count <= (int)state.Length then Some(count)
                else None

            member x.getUserState() = userState

            member x.setUserState s = userState <- s
            
            member x.position () = startPos
        
        member x.initBytes size = Array.create size <| byte(0)

        member x.seekToEnd() = state.Seek((int64)0, SeekOrigin.End) |> ignore

        member x.streamStartsWith (input:IStreamP<Stream, byte[], _> ) bytes =                 
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

    let makeBinStream stream = new BinStream<unit>(stream, ())

    let toInterface binstream = binstream :> IStreamP<Stream, byte[], unit>

