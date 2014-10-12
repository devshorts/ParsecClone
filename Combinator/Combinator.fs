namespace ParsecClone.CombinatorBase

open System
open System.IO
open System.Reflection

[<AutoOpen>]
module Combinator =      

    exception Error of string          

    type ParserMetadata = {
        IsChoice:bool
    }

    type State<'StateType, 'ConsumeType, 'UserState> = IStreamP<'StateType, 'ConsumeType, 'UserState>

    type Reply<'Return, 'StateType, 'ConsumeType, 'UserState> = 'Return option * State<'StateType, 'ConsumeType, 'UserState>

    type Parser<'Return, 'StateType, 'ConsumeType, 'UserState> = State<'StateType, 'ConsumeType, 'UserState> -> Reply<'Return, 'StateType, 'ConsumeType, 'UserState>  
        
    let preturn value = fun stream -> (Some(value), stream)
        
    let pzero = fun stream -> (None, stream)

    let mutable enableDebug = false

    let succeed parser state = 
        match parser state with
            | (None, _) -> (false, state)
            | (_, newState) -> (true, newState)
    
    let opt current =
        fun s ->
            let (result, state) = current s
            match result with 
                | Some(m) -> (Some(Some(m)), state)
                | None -> (Some(None), state)

    let getAltReply first second inputState =       

        let (result, state:State<_,_,_>) as reply = first inputState

        // if the first one matches, stop
        match result with 
            | Some(_) -> reply 
            | None  when state.equals inputState -> second inputState            
            | None ->  failwith "No match found and underlying state was modified"

    let getBacktrackReply current (next:OptimizedClosures.FSharpFunc<_,_,_>) input =
        let (result, state:State<_,_,_>) as reply = current input
        match result with 
            | Some(r) ->  try next.Invoke(r, state) with | e -> state.backtrack(); reply
            | None when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | None -> (None, state)      

    let getReply current (next:OptimizedClosures.FSharpFunc<_,_,_>) (input:State<_,_,_>) =       
        let (result, state:State<_,_,_>) = current input
        match result with 
            | Some(r) -> next.Invoke(r, state)
            | None when not (state.equals input) -> failwith "No match found and underlying state was modified"
            | None -> (None, state)

    let inline private adapt x = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(x)

    let (>>=) (current) (next)  = getReply current (adapt next)

    let (>>=?) (current) (next) = getBacktrackReply current (adapt next)
        
    let (|>>) parser targetType = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType = parser >>= fun _ -> preturn targetType

    let (>>--) parser wrapper = 
        fun state -> 
            wrapper (fun () -> parser state)

    let (>>.)  parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn second

    let (.>>)  parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn first    
        
    let (>>|.) parser1 transformer = parser1  >>= fun result -> preturn (transformer result)      

    let (.>>.) parser1 parser2 = 
        parser1 >>= fun first -> 
        parser2 >>= fun second -> 
        preturn (first, second)

    let (<|>) parser1 parser2 = getAltReply parser1 parser2        

    let matcher eval target =  
        let evalOpt = adapt eval       
        fun (currentState: State<_,_,_>) -> 
            match evalOpt.Invoke(currentState, target) with
                | Some(amount) -> currentState.consume amount                        
                | None         -> (None, currentState)
      
    let skipper eval target =   
        let evalOpt = adapt eval      
        fun (currentState: State<_,_,_>) -> 
            match evalOpt.Invoke(currentState, target) with
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
                            | Some(result), nextState when not <| predicate (result::resultList)  -> many' parser (result::resultList, nextState)
                            | Some(_), _ ->  currentState.backtrack(); returnValue()
                            | _ ->  returnValue()
                        
                                  
            many' parser ([], state)

    let takeTill predicate (parser) = takeTillWithCount 0 (fun i -> predicate <| List.head i) parser           

    let takeWhile predicate (parser) = takeTill (predicate >> not) parser
   
    (* 
        Take the parser while the end parser does not succeed. When the end parser succeeds
        validate that how much was taken matches the minCount and returns a list of results 
    *)
    let private manyTillWithCount minCount parser endParser = 
        fun state ->    
            let rec many' (currentState:State<_,_,_>) acc =         
                if not <| currentState.hasMore() then
                    (None, currentState)
                else               
                    let (result, nextState) as reply = parser currentState
                    match result with 
                        | Some(m) ->                                                                             
                            match succeed endParser nextState with 
                                | false, _ -> many' nextState (m::acc)
                                | true, consumedEndParserState -> (Some(m::acc), consumedEndParserState)

                        | None -> 
                            match List.length acc with
                                | x when x >= minCount -> (Some(acc), nextState)                                
                                | _ -> (None, nextState)
    
            
            many' state []
    
    let manyTill p pEnd = manyTillWithCount 0 p pEnd

    let manyTill1 p pEnd = manyTillWithCount 1 p pEnd
        
    let manyN num (parser) =                              
        takeTillWithCount 0 (fun i -> List.length i > num) parser    >>= fun result -> 
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
        List.foldBack (fun acc value -> acc <|> value) parsers pzero

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
            //printfn "executing user state parser"    
            if predicate (state.getUserState()) then
                parser state
            else
                None, state


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
