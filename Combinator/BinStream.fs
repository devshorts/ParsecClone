namespace ParsecClone.BinaryCombinator 

open System.IO
open System.Collections.Generic
open System.Reflection
open System
open ParsecClone.CombinatorBase

[<AutoOpen>]
module BinStreams =     
   
    type BinArgs = {
        // key = stream start position
        // value = list of byte arrays that could have been read from that position (it would've been more space efficient
        //          to track deltas or munge things, but realistically there should only be one element here

        cache: Dictionary<int64, byte[] list> option
    }

    let inline initBytes size = Array.create size <| byte(0)

    let private seekTo  (stream:Stream) location = stream.Seek(location, SeekOrigin.Begin) |> ignore

    let private readFromStream count (stream:Stream) = 
            let b = initBytes count

            if Combinator.enableDebug then
                printfn "reading %d from state position %d" count (stream.Position)
            
            stream.Read(b, 0, count) |> ignore

            b

    let createCache() = { cache = Some (new Dictionary<int64, byte[] list>()) }

    let toInterface binstream = binstream :> IStreamP<Stream, byte[], unit>

    type BinStream<'UserState> (state:Stream, userState:'UserState, args: BinArgs) =           
        let mutable userState = userState

        let startPos = state.Position

        member x.clone () = new BinStream<'UserState>(state, userState, args) :> IStreamP<Stream, byte[], 'UserState> 
            
        interface IStreamP<Stream, byte[],'UserState>  with     
              
            member x.state = state     

            member x.consume (count) =           
                // make sure to advance the underlying stream position
                // if we pulled data back from the cache
                      
                let result = 
                    Option.bind (fun (fromCache, bytes) -> 
                        if fromCache then
                            seekTo state (startPos + (int64)(Array.length bytes))
                        Some(bytes)) (x.consumeOrGet count)

                (result, x.clone())

            member x.skip count = 
                state.Seek((int64)count, SeekOrigin.Current) |> ignore

                (Some(true),  new BinStream<'UserState>(state, userState, args) :> IStreamP<Stream, byte[], 'UserState> )

            member x.backtrack () =
                if Combinator.enableDebug then
                    printfn "backtracking to position %d" startPos
             
                state.Seek(startPos, SeekOrigin.Begin) |> ignore          

            member x.hasMore () = state.Position <> state.Length

            member x.equals istream = istream.state.Position = startPos

            member x.canConsume count = 
                if (int)state.Position + (int)count <= (int)state.Length then Some(count)
                else None

            member x.getUserState() = userState

            member x.setUserState s = userState <- s
            
            member x.position () = state.Position

        member x.seekToEnd() = state.Seek((int64)0, SeekOrigin.End) |> ignore

        member x.seekTo n = state.Seek(n, SeekOrigin.Begin) |> ignore

        member x.consumeOrGet count = 

            match x.testCache startPos count with
                | Some(bytes) -> Some(true, bytes)
                | None -> 
                    state 
                        |> readFromStream count
                        |> fun(bytes) ->     
                                                
                            x.updateCache startPos bytes

                            Some(false, bytes)

        member x.streamStartsWith bytes =                 
            if (int)state.Position + (Array.length bytes) > (int)state.Length then
                None
            else 
                let start = state.Position

                let count = Array.length bytes

                let result = 
                    match x.consumeOrGet count with
                        | None -> None
                        | Some(_, values) -> 
                            if values = bytes then 
                                Some(count) 
                            else None

                seekTo state start

                result
                                            
          member x.testCache startPosition length = 
                match args.cache with 
                    | None -> None
                    | Some(cache) ->
                        let (found, list) = cache.TryGetValue(startPosition)

                        if not found then None
                        else 
                            List.tryFind (Array.length >> (=) length) list

          member x.updateCache startPosition bytes = 
                match args.cache with 
                    | None -> ()
                    | Some(cache) -> 
                        let (found, list) = cache.TryGetValue(startPosition)

                        if not found then                    
                            cache.[startPosition] <- [bytes]                  
                        else                     
                            let sameBytes = List.tryFind (Array.length >> (=) (Array.length bytes)) list
                            match sameBytes with
                                | Some(_) -> () // do nothing, already read that much!
                                | None -> cache.[startPosition] <- bytes::list

    let makeBinStream stream = new BinStream<unit>(stream, (), createCache())








       