namespace Combinator

module Combinator =
    
    exception Error of string          

    type State<'StateType, 'ConsumeType> = IStreamP<'StateType, 'ConsumeType>

    type Reply<'Return, 'StateType, 'ConsumeType> = 'Return option * State<'StateType, 'ConsumeType>

    type Parser<'Return, 'StateType, 'ConsumeType> = State<'StateType, 'ConsumeType> -> Reply<'Return, 'StateType, 'ConsumeType>  
    
    let preturn value : Parser<'Return, 'StateType, 'ConsumeType> = fun stream -> (Some(value), stream)
        
    let pzero = fun stream -> (None, stream)
        
    let getOptionReply (current) (parser : Parser<'Return, 'StateType, 'ConsumeType>) (input) =
        let match1 = current input
        match match1 with 
            | (Some(m), _) -> match1
            | (None, state) when state = input -> parser input
            | (None, state) -> raise (Error("No match found during OR and underlying state was modified")) 

    let getBacktrackReply current next input =
        let match1 = current input
        match match1 with 
            | (Some(result), (state:IStreamP<'A, 'B>)) -> 
                try
                    state |> next result                              
                with
                    | e -> 
                        state.backtrack()
                        match1
            | (None, state) when state <> input -> raise (Error("No match found and underlying state was modified")) 
            | (None, state) -> (None, state)      

    let getReply current next input : Reply<'Return, 'StateType, 'ConsumeType> =
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> state |> next result                              
            | (None, state) when state <> input -> raise (Error("No match found and underlying state was modified")) 
            | (None, state) -> (None, state)

    let (>>=)  (current) (next)  : Parser<'Return, 'StateType, 'ConsumeType> = getReply current next                                   

    let (>>=?) (current) (next) = getBacktrackReply current next       
        
    let (|>>)  parser targetType : Parser<'Return, 'StateType, 'ConsumeType> = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType : Parser<'Return, 'StateType, 'ConsumeType> = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn second

    let (.>>)  parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn first

    let (.>>.) parser1 parser2 : Parser<'Return * 'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn (first, second)

    let (<|>) parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = getOptionReply parser1 parser2        

    let matcher eval target =         
        fun currentState -> 
            match eval (currentState:State<'StateType, 'ConsumeType>) target with
                | Some(amount) -> currentState.consume currentState amount                        
                | None         -> (None, currentState)
        
    
    let many (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =        
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

        

    let manyN num (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =         
        fun state ->
            let rec many' parser (found, currentState) num =                                        
                match parser currentState with
                    | (Some(m), nextState) when num > 0 ->                                    
                        many' parser (m::found, nextState) (num - 1)
                    | _ -> 
                        if num <> 0 then 
                            raise(Error("Error"))//(None, currentState) 
                        else
                            if List.length found > 0 then
                                (Some(List.rev found), currentState)          
                            else
                                (None, currentState)
                                  
            many' parser ([], state) num

        

    let anyOf comb = List.fold (fun acc value -> acc <|> comb value) pzero
     
    let choice parsers  : Parser<'Return, 'StateType, 'ConsumeType> = 
        parsers |> List.fold (fun acc value -> acc <|> value) pzero

    let attempt (parser) : Parser<'Return, 'StateType, 'ConsumeType> =         
        fun state ->
            let backtrack () = state.backtrack()
                               (None, state)

            try
                let result = parser state 
                match result with
                    | (Some(_), _) -> result
                    | _ -> backtrack()
            with
                | e -> backtrack()
        

    let test input (parser: Parser<'Return,_,_>) = 
        match parser input with
            | (Some(m), _) -> m
            | (None, _) -> raise (Error("No matches"))
