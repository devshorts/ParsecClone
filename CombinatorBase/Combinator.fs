namespace Combinator

[<AutoOpen>]
module Combinator =
    
    exception Error of string          

    type State<'StateType, 'ConsumeType> = IStreamP<'StateType, 'ConsumeType>

    type Reply<'Return, 'StateType, 'ConsumeType> = 'Return option * State<'StateType, 'ConsumeType>

    type Parser<'Return, 'StateType, 'ConsumeType> = State<'StateType, 'ConsumeType> -> Reply<'Return, 'StateType, 'ConsumeType>  
    
    let preturn value : Parser<'Return, 'StateType, 'ConsumeType> = fun stream -> (Some(value), stream)
        
    let pzero = fun stream -> (None, stream)
        
    let opt current =
        fun s ->
            let match1 = current s
            match match1 with 
                | (Some(result), state) -> (Some(Some(result)), state)
                | (None, state) -> (Some(None), state)

    let getAltReply first second inputState  =
        let match1 = first inputState

        // if the first one matches, stop
        match match1 with 
            | (Some(m), _) -> match1 
            | (None, state)  when state = inputState -> second inputState            
            |_ ->  raise (Error("No match found and underlying state was modified"))  

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

    let getReply current next (input:IStreamP<_,_>) : Reply<'Return, 'StateType, 'ConsumeType> =       
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

    let (.>>.) parser1 parser2 : Parser<_, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn (first, second)

    let (<|>) parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = getAltReply parser1 parser2        

    let matcher eval target =         
        fun currentState -> 
            match eval (currentState:State<'StateType, 'ConsumeType>) target with
                | Some(amount) -> currentState.consume amount                        
                | None         -> (None, currentState)
            
    
    let takeTill predicate (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =        
        fun state ->
            let retTest found currentState = 
                if List.length found > 0 then
                    (Some(List.rev found), currentState)          
                else
                    (None, currentState)

            let rec many' parser (found, currentState:IStreamP<'A, 'B>) =              
                let returnValue() = retTest found currentState

                if not (currentState.hasMore()) then
                    returnValue()
                else
                    match parser currentState with
                        | (Some(m), (nextState:IStreamP<'A, 'B>)) ->                                                 
                            if not (predicate m) then                                   
                                many' parser (m::found, nextState)
                            else 
                                currentState.backtrack()
                                returnValue()

                        | _ ->  returnValue()
                                  
            many' parser ([], state)

    let takeWhile predicate (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =  takeTill (predicate >> not) parser
        
    let manyN num (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =         
        let count = ref 0
        let countReached _ = count := 1 + !count
                             !count > num

        takeWhile (countReached >> not) parser >>= 
                                            fun result -> 
                                               if result.Length <> num then
                                                    raise(Error("Error, attempted to match " + num.ToString() + " but only got " + (result.Length).ToString())) 
                                               preturn result                                               
                                
     
    
    let eof = 
        fun (state:IStreamP<_,_>) -> 
            if state.hasMore() then
                raise(Error("EOF wasn't matched"))
            else 
                (Some(()), state)

    let many (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =  takeWhile (fun s -> true) parser   

    let anyOf comb = List.fold (fun acc value -> acc <|> comb value) pzero
     
    let choice parsers  : Parser<'Return, 'StateType, 'ConsumeType> = 
        parsers |> List.fold (fun acc value -> acc <|> value) pzero

    let between ``open`` parser close = ``open`` >>. parser .>> close

    let manySatisfy = takeWhile 
       
    let optWith parser listParser =
        parser >>= fun result1 ->
        listParser >>= 
        function 
        | Some(results) -> preturn (result1::results)
        | None -> preturn (result1::[])

    let (.<?>.) parser listParser = optWith (opt parser) (opt listParser)

    let satisfy predicate parser = 
        fun (state : IStreamP<_,_>) ->  
            let result = parser state                                 
            match result with
                | (Some(m), nextState) ->
                    if predicate m then
                        result
                    else
                        state.backtrack()
                        (None, state)
                | _ -> result       

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
        

    let sepBy p1 p2 = (attempt (p1 .>> p2) <|> p1)

    let test input (parser: Parser<'Return,_,_>) = 
        match parser input with
            | (Some(m), _) -> m
            | (None, _) -> raise (Error("No matches"))
