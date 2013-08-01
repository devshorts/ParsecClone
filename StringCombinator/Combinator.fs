namespace Combinator

module Combinator =
    
    exception Error of string          

    type State<'Y> = IStreamP<'Y>

    type Reply<'T, 'Y> = 'T option * State<'Y>

    type Parser<'T, 'Y> = State<'Y> -> Reply<'T, 'Y>  
    
    let preturn value : Parser<'T, 'Y> = 
        let p : Parser<'T, 'Y> = fun stream -> (Some(value), stream)
        p

    let pzero : Parser<'T, 'Y> = 
        let p: Parser<'T, 'Y> = fun stream -> (None, stream)
        p

    let getOptionReply (current:Parser<'B, 'Y>) (parser:Parser<'A, 'Y>) (input : State<'Y>): Reply<'A, 'Y> =
        let match1 = current input
        match match1 with 
            | (Some(m), _) -> match1
            | (None, state) when state = input -> parser input
            | (None, state) -> raise (Error("No match found during OR and underlying state was modified")) 

    let getReply (current:Parser<'B, 'Y>) (next:'B -> Parser<'A, 'Y>)  (input : State<'Y>): Reply<'A, 'Y> =
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> 
                let parser2 = next result
                parser2 state                    
            | (None, state) when state <> input -> 
                raise (Error("No match found and underlying state was modified")) 
            | (None, state) -> (None, state)

    let (>>=)  (current:Parser<'B, 'Y>) (next:'B -> Parser<'A, 'Y>)  : Parser<'A, 'Y> = getReply current next                                   

    let (>>=?) (current:Parser<'B, 'Y>) (next:'B -> Parser<'A, 'Y>) : Parser<'A, 'Y> = 
        fun state ->
            try
                getReply current next state
            with
                | e -> (None, state)
        
    let (|>>)  parser targetType : Parser<'T, 'Y> = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType : Parser<'T, 'Y> = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 : Parser<'T, 'Y> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn second

    let (.>>)  parser1 parser2 : Parser<'T, 'Y> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn first

    let (.>>.) parser1 parser2 : Parser<'a * 'b, 'Y> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn (first, second)

    let (<|>) parser1 parser2 : Parser<'T, 'Y> = getOptionReply parser1 parser2        

    let matcher eval consume target: Parser<'T, 'Y> = 
        let p : Parser<'T, 'Y> = 
            fun currentState -> 
                match eval currentState target with
                    | Some(value) -> consume currentState value
                        //Some(result : 'Y) -> (Some(result), (currentState.consume result))
                    | None -> (None, currentState)
        p
    
    let many (parser : Parser<'T, 'Y>) = 
        let p : Parser<'T list, 'Y> = 
            fun state ->
                let rec many' parser (found, currentState) =                    
                    match parser currentState with
                        | (Some(m), nextState) ->                                    
                            many' parser (m::found, nextState)
                        | _ -> 
                            if List.length found > 0 then
                                (Some(List.rev found), currentState)          
                            else
                                (None, currentState)
                                  
                many' parser ([], state)

        p

    let manyN num (parser : Parser<'T, 'Y>) = 
        let p : Parser<'T list, 'Y> = 
            fun state ->
                let rec many' parser (found, currentState) num =                                        
                    match parser currentState with
                        | (Some(m), nextState) when num > 0 ->                                    
                            many' parser (m::found, nextState) (num - 1)
                        | _ -> 
                            if List.length found > 0 then
                                (Some(List.rev found), currentState)          
                            else
                                (None, currentState)
                                  
                many' parser ([], state) num

        p

    let anyOf comb = List.fold (fun acc value -> acc <|> comb value) pzero

    let choice (parsers : Parser<'T, 'Y> list) = 
        parsers |> List.fold (fun acc value -> acc <|> value) pzero

    let attempt (parser : Parser<'T, 'Y>) = 
        let p : Parser<'T, 'Y> = 
            fun state ->
                try
                    let result = parser state 
                    match result with
                        | (Some(_), _) -> result
                        | _ -> (None, state)
                with
                    | e -> (None, state)
        p

    let test input (parser:Parser<'T, 'Y>) = 
        match parser input with
            | (Some(m), _) -> m
            | (None, _) -> raise (Error("No matches"))
