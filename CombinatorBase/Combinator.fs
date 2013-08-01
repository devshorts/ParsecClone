namespace Combinator

module Combinator =
    
    exception Error of string          

    type State<'Y, 'A> = IStreamP<'Y, 'A>

    type Reply<'T, 'Y, 'A> = 'T option * State<'Y, 'A>

    type Parser<'T, 'Y, 'A> = State<'Y, 'A> -> Reply<'T, 'Y, 'A>  
    
    let preturn value = fun stream -> (Some(value), stream)
        
    let pzero = fun stream -> (None, stream)
        
    let getOptionReply (current) (parser) (input) =
        let match1 = current input
        match match1 with 
            | (Some(m), _) -> match1
            | (None, state) when state = input -> parser input
            | (None, state) -> raise (Error("No match found during OR and underlying state was modified")) 

    let getReply current next input =
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> state |> next result                              
            | (None, state) when state <> input -> raise (Error("No match found and underlying state was modified")) 
            | (None, state) -> (None, state)

    let (>>=)  (current) (next) = getReply current next                                   

    let (>>=?) (current) (next) = 
        fun (state:IStreamP<'T, 'Y>) ->
            try
                getReply current next state
            with
                | e -> 
                    state.backtrack()
                    (None, state)
        
    let (|>>)  parser targetType = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn second

    let (.>>)  parser1 parser2 = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn first

    let (.>>.) parser1 parser2 = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn (first, second)

    let (<|>) parser1 parser2 = getOptionReply parser1 parser2        

    let matcher eval target =         
        fun currentState -> 
            match eval (currentState:IStreamP<'T, 'Y>) target with
                | Some(amount) -> currentState.consume currentState amount                        
                | None         -> (None, currentState)
        
    
    let many (parser) =        
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

        

    let manyN num (parser) =         
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

        

    let anyOf comb = List.fold (fun acc value -> acc <|> comb value) pzero

    let choice parsers = 
        parsers |> List.fold (fun acc value -> acc <|> value) pzero

    let attempt (parser) =         
        fun (state:IStreamP<'T, 'Y>) ->
            let backtrack () = state.backtrack()
                               (None, state)

            try
                let result = parser state 
                match result with
                    | (Some(_), _) -> result
                    | _ -> backtrack()
            with
                | e -> backtrack()
        

    let test input parser = 
        match parser input with
            | (Some(m), _) -> m
            | (None, _) -> raise (Error("No matches"))
