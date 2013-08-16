namespace Combinator

[<AutoOpen>]
module Combinator =
    
    exception Error of string          

    type State<'StateType, 'ConsumeType> = IStreamP<'StateType, 'ConsumeType>

    type Reply<'Return, 'StateType, 'ConsumeType> = 'Return option * State<'StateType, 'ConsumeType>

    type Parser<'Return, 'StateType, 'ConsumeType> = State<'StateType, 'ConsumeType> -> Reply<'Return, 'StateType, 'ConsumeType>  
    
    let preturn value : Parser<'Return, 'StateType, 'ConsumeType> = fun stream -> (Some(value), stream)
        
    let pzero = fun stream -> (None, stream)

    let succeed parser state = 
        match parser state with
            | (None, _) -> (false, state)
            | (_, newState) -> (true, newState)
    
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
            | (None, state : IStreamP<_,_>)  when state.equals inputState -> second inputState            
            |_ ->  failwith "No match found and underlying state was modified"

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
            | (None, state) when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | (None, state) -> (None, state)      

    let getReply current next (input:IStreamP<_,_>) : Reply<'Return, 'StateType, 'ConsumeType> =       
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> state |> next result                              
            | (None, state : IStreamP<_,_>) when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | (None, state) -> (None, state)

    let (>>=)  (current) (next)  : Parser<'Return, 'StateType, 'ConsumeType> = getReply current next                                   

    let (>>=?) (current) (next) = getBacktrackReply current next       
        
    let (|>>)  parser targetType : Parser<'Return, 'StateType, 'ConsumeType> = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType : Parser<'Return, 'StateType, 'ConsumeType> = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn second

    let (.>>)  parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn first

    let (>>..)  parser1 applier : Parser<'Return, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> applier first        

    let (.>>.) parser1 parser2 : Parser<_, 'StateType, 'ConsumeType> = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn (first, second)

    let (<|>) parser1 parser2 : Parser<'Return, 'StateType, 'ConsumeType> = getAltReply parser1 parser2        

    let matcher eval target =         
        fun currentState -> 
            match eval (currentState:State<'StateType, 'ConsumeType>) target with
                | Some(amount) -> currentState.consume amount                        
                | None         -> (None, currentState)
            
   
     
    let private takeTillB predicate parser minCount = 
        fun state ->
            let didFind found currentState = 
                if List.length found > 0 then
                    (Some(List.rev found), currentState)          
                else
                    if List.length found < minCount then 
                        failwith ("Needed to consume at least " + minCount.ToString() + " element but did not")
                    else                
                        (None, currentState)

            let rec many' parser (returnList, currentState:IStreamP<'A, 'B>) =              
                let returnValue() = didFind returnList currentState

                if not (currentState.hasMore()) then
                    returnValue()
                else
                    match parser currentState with
                        | (Some(m), (nextState:IStreamP<'A, 'B>)) ->                                                 
                            if not (predicate m) then                                   
                                many' parser (m::returnList, nextState)
                            else 
                                currentState.backtrack()
                                returnValue()

                        | _ ->  returnValue()
                                  
            many' parser ([], state)

    let takeTill predicate (parser) : Parser<'Return list, 'StateType, 'ConsumeType> = takeTillB predicate parser 0          

    let takeWhile predicate (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =  takeTill (predicate >> not) parser
        
    let private manyTillB minCount (parser:Parser<_,_,_>) (parserEnd:Parser<_,_,_>) : Parser<_,_,_> = 
        fun state ->
            let rec take (currentState:IStreamP<_,_>) acc = 
                if not (currentState.hasMore()) then
                    (None, state)
                else                    
                    let result = parser currentState

                    match result with 
                        | Some(m), newState -> 
                            let (suceeded, s) = succeed parserEnd newState

                            if not suceeded then 
                                take newState (m::acc)
                            else 
                                (Some(m::acc), s)

                        | (None, newState) -> 
                            if List.length acc < minCount then
                                (None, newState)
                            else
                                failwith "Needed to match one more of parser, but found zero"

            take state []

    let manyTill p pEnd = manyTillB 0 p pEnd

    let manyTill1 p pEnd = manyTillB 1 p pEnd
        
    let manyN num (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =                      
        let count = ref 0

        let countReached _ = 
            count := 1 + !count
            !count > num

            
        takeWhile (countReached >> not) parser >>= fun result -> 
            if result.Length <> num then
                raise(Error("Error, attempted to match " + num.ToString() + " but only got " + (result.Length).ToString())) 
            preturn result          
    
    let eof = 
        fun (state:IStreamP<_,_>) -> 
            if state.hasMore() then
                raise(Error("EOF wasn't matched"))
            else 
                (Some(()), state)

    let lookahead p : Parser<_,_,_> = 
        fun (state:IStreamP<_,_>) ->
            let result = p state

            state.backtrack()

            (fst result, state)
                                
    let many (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =  takeWhile (fun s -> true) parser   

    let many1 (parser) : Parser<'Return list, 'StateType, 'ConsumeType> =  takeTillB (fun s -> false) parser 1  

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

    let optWithList listParser parser =
        listParser >>= fun results ->
        parser >>= 
        function 
        | Some(result) -> preturn (results::[result])
        | None -> preturn (results::[])


    let (.<?>>.) parser listParser = optWith (opt parser) (opt listParser)

    let (.<<?>.) parser listParser = optWith (opt listParser) (opt parser)

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
               
    let choiceAttempts parsers = 
        match List.rev parsers with 
            | h::[] -> h
            | h::t -> (List.fold (fun acc value -> acc <|> attempt value) pzero t) <|> h
            | [] -> failwith "Can't attempt on empty parser list"


    let sepBy p1 p2 = (attempt (p1 .>> p2) <|> p1)

    let test input (parser: Parser<'Return,_,_>) = 
        match parser input with
            | (Some(m), s) -> m
            | (None, _) -> failwith "No matches"
