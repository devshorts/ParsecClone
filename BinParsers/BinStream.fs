namespace BinParsers

open System.IO
open Combinator


type BinStream (state:Stream) = 
    interface IStreamP<Stream> with       
        member this.state = state
        