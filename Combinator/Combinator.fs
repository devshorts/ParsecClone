namespace ParsecClone.CombinatorBase

[<AutoOpen>]
module Combinator =
    
    exception Error of string          

    type State<'StateType, 'ConsumeType, 'UserState> = IStreamP<'StateType, 'ConsumeType, 'UserState>

    type Reply<'Return, 'StateType, 'ConsumeType, 'UserState> = 'Return option * State<'StateType, 'ConsumeType, 'UserState>

    type Parser<'Return, 'StateType, 'ConsumeType, 'UserState> = State<'StateType, 'ConsumeType, 'UserState> -> Reply<'Return, 'StateType, 'ConsumeType, 'UserState>  
    
    let preturn value = fun stream -> (Some(value), stream)
        
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
            | (None, state : State<_,_,_>)  when state.equals inputState -> second inputState            
            | (None, state) ->  failwith "No match found and underlying state was modified"

    let getBacktrackReply current (next:OptimizedClosures.FSharpFunc<_,_,_>) input =
        let match1 = current input
        match match1 with 
            | (Some(result), (state:State<_,_,_>)) -> 
                try
                    next.Invoke(result, state)
                with
                    | e -> 
                        state.backtrack()
                        match1
            | (None, state) when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | (None, state) -> (None, state)      

    let getReply current (next:OptimizedClosures.FSharpFunc<_,_,_>) (input:State<_,_,_>) =       
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> next.Invoke(result, state)
            | (None, state : State<_,_,_>) when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | (None, state) -> (None, state)

    let inline private adapt x = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(x)

    let (>>=) (current) (next)  = getReply current (adapt next)

    let (>>=?) (current) (next) = getBacktrackReply current (adapt next)
        
    let (|>>) parser targetType = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn second

    let (.>>)  parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn first

    let (>>..)  parser1 applier = 
        parser1 >>= fun first -> applier first  
        
    let (>>|.) parser1 transformer = parser1  >>= fun result -> preturn (transformer result)      

    let (.>>.) parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn (first, second)

    let (<|>) parser1 parser2 = getAltReply parser1 parser2        

    let matcher eval target =         
        fun currentState -> 
            match eval (currentState:State<_,_,_>) target with
                | Some(amount) -> currentState.consume amount                        
                | None         -> (None, currentState)
      
    let skipper eval target =         
        fun currentState -> 
            match eval (currentState:State<_,_,_>) target with
                | Some(amount) -> currentState.skip amount                        
                | None         -> (None, currentState)      
   
     
    (* 
        Take parser till predicate is true and validate the mininum number of elements was found
    *)
    let private takeTillWithCount minCount predicate parser = 

        let didFind found currentState = 
                match List.length found with 
                    | x when x > 0 && x >= minCount -> (Some(List.rev found), currentState) 
                    | x when x < minCount -> failwith ("Needed to consume at least " + minCount.ToString() + " element but did not")
                    | _ -> (None, currentState)
               
        fun state ->
            let rec many' parser (resultList, currentState:State<_,_,_>) =              
                let returnValue() = didFind resultList currentState

                match currentState.hasMore() with
                    | false -> returnValue()
                    | true ->
                        match parser currentState with
                            | Some(result), nextState when not <| predicate result  -> many' parser (result::resultList, nextState)
                            | Some(_), _ ->  currentState.backtrack(); returnValue()
                            | _ ->  returnValue()
                                  
            many' parser ([], state)

    let takeTill predicate (parser) = takeTillWithCount 0 predicate parser           

    let takeWhile predicate (parser) = takeTill (predicate >> not) parser
   
    (* 
        Take the parser while the end parser does not succeed. When the end parser succeeds
        validate that how much was taken matches the minCount and returns a list of results 
    *)
    let private manyTillWithCount minCount parser endParser = 
        fun state ->            
            let ifHasMore apply (state:State<_,_,_>) acc = 
                match state.hasMore() with 
                    | true -> apply state acc
                    | false -> None, state

            let rec testStream state acc = ifHasMore takeParser state acc    
                                
            and takeParser currentState acc = 
                match parser currentState with 
                    | Some(m), consumedParserState ->                                                                             
                        match succeed endParser consumedParserState with 
                            | false, _ -> testStream consumedParserState (m::acc)
                            | true, consumedEndParserState -> (Some(m::acc), consumedEndParserState)

                    | (None, newState) -> 
                        match List.length acc with
                            | x when x < minCount -> (None, newState)
                            | x when x > 0 -> (Some(acc), newState)
                            | _ -> (None, newState)
    
            
            testStream state []

    let manyTill p pEnd = manyTillWithCount 0 p pEnd

    let manyTill1 p pEnd = manyTillWithCount 1 p pEnd
        
    let manyN num (parser) =                      
        let count = ref 0

        let countReached _ = 
            count := 1 + !count
            !count > num

            
        takeWhile (countReached >> not) parser >>= fun result -> 
            if result.Length <> num then
                failwith ("Error, attempted to match " + num.ToString() + " but only got " + result.Length.ToString())

            preturn result          
    
    let exactly = manyN
        
    let eof = 
        fun (state:State<_,_,_>) -> 
            if state.hasMore() then
                (None, state)
            else 
                (Some(()), state)

    let lookahead p = 
        fun (state:State<_,_,_>) ->
            let result = p state

            state.backtrack()

            (fst result, state)
                                
    let many (parser) =  takeWhile (fun s -> true) parser   

    let many1 (parser) =  takeTillWithCount 1 (fun s -> false) parser  

    let anyOf comb = List.fold (fun acc value -> acc <|> comb value) pzero
     
    let choice parsers = 
        parsers |> List.fold (fun acc value -> acc <|> value) pzero

    let between ``open`` parser close = ``open`` >>. parser .>> close

    let between2 bookend parser = between bookend parser bookend

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

    let private backtrackNone (state:State<_,_,_>) = 
        state.backtrack()
        None, state   

    let satisfyUserState predicate parser = 
        fun (state : State<_,_,'UserState>) ->  
            let (r, nextState:State<_,_,_>) as result = parser state

            match r with 
                | Some(m) when predicate (state.getUserState()) -> result 
                | Some(_) -> backtrackNone state                                                                     
                | _ -> None, nextState

    let satisfy predicate parser = 
        fun (state : State<_,_,_>) ->  
            let (r, nextState) as result = parser state
            match r with 
                | Some(m) when predicate m -> result 
                | Some(_) -> backtrackNone state                                                     
                | _ -> None, nextState                                             

    let attempt (parser) =         
        fun state ->
            
            try
                let result = parser state 
                match result with
                    | (Some(_), _) -> result
                    | _ -> backtrackNone state
            with
                | e -> backtrackNone state
               
    let choiceAttempts parsers = 
        match List.rev parsers with 
            | h::[] -> h
            | h::t -> (List.fold (fun acc value -> acc <|> attempt value) pzero t) <|> h
            | [] -> failwith "Can't attempt on empty parser list"


    let sepBy p1 p2 = (attempt (p1 .>> p2) <|> p1)

    let createParserForwardedToRef()= 
        let refParser = ref (fun state -> failwith "Forwarded ref parser was never initialized")

        let fwdParser : Parser<_,_,_,_>  = 
            fun s -> 
                !refParser s

        (fwdParser, refParser)


    (*
        Elevates the seed to a new stream and applies the parser 
        to the seeded stream state
    *)
    let reproc elevator seed parser = 
        fun stream ->
            let result = seed stream
            match result with
                | Some(m), (postSeedState:State<_,_,_>) -> 
                    let elevatedStream = elevator m (postSeedState.getUserState())
                    
                    let (parseResult, _) = parser elevatedStream

                    (parseResult, postSeedState)                        
                                        
                | (None, consumed) -> (None, consumed)


    let getUserState (state:State<_,_,_>) = (Some(state.getUserState()), state)

    let setUserState value (state:State<_,_,_>) = (Some(state.setUserState value), state)

    let statePosition = 
        fun (state:State<_,_,_>) -> 
            (Some(state.position()), state)

    // ------------------------------
    // Computation expression syntax
    // ------------------------------
    [<Sealed>]
    type ParserCombinator() =
        member t.Delay getParser = fun (stream:State<_,_,_>) -> getParser() <| stream
        member t.Return(x) = preturn x
        member t.Bind(parser, next) = parser >>= next
        member t.Zero() = pzero
        member t.ReturnFrom p = p
        
        member t.TryWith parser (customFail: exn -> Parser<_,_,_,_>) =
            fun stream ->
                try parser stream with exn -> (customFail exn) stream

        member t.TryFinally parser finalBlock =
            fun stream ->
                try parser stream finally finalBlock()

    let parse = ParserCombinator()

    let test (input:State<_,_,'UserState>) (parser:Parser<_,_,_,'UserState>) = 
        match parser input with
            | (Some(m), s) -> m
            | (None, _) -> failwith "No matches"
