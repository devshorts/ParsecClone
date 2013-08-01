namespace BinParsers 

open Combinator.Combinator
open Combinator
open System.IO


module BinParser = 
    
    type ParseState = State<Stream>

    let streamCanBeConsumed = 
                (fun count (state:ParseState) ->
                    if (int)state.state.Length >= count then
                        Some((int)count)
                    else 
                        None)

    let consumeStream = 
                (fun (state:ParseState) (count) ->
                    let mutable bytes = Array.init count (fun i -> byte(0))
                    state.state.Read(bytes, 0, count) |> ignore
                    (Some(bytes), new BinStream(state.state) :> IStreamP<Stream>))

    let binMatch (num:int) = 
        let p : Parser<'T, 'Y> = matcher streamCanBeConsumed consumeStream num
        p

    let byte1<'a> = binMatch 1 >>= fun b1 -> preturn b1.[0]

    let byte2<'a> = byte1 >>= fun b1 -> byte1 >>= fun b2 -> preturn [|b1;b2|]    

