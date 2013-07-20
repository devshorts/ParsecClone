namespace StringCombinator


module Combinator =
    
    exception Error of string   
    
    type State = string

    type Reply<'T> = 'T option * State

    type Parser<'T> = State -> Reply<'T>  
    
    let preturn value : Parser<'T> = 
        let p : Parser<'T> = fun stream -> (Some(value), stream)
        p

    let pzero : Parser<'T> = 
        let p: Parser<'T> = fun stream -> (None, stream)
        p

    let getOptionReply (current:Parser<'B>) (parser:Parser<'A>) (input : State): Reply<'A> =
        let match1 = current input
        match match1 with 
            | (Some(m), _) -> 
                System.Console.WriteLine("found1 {0}", m.ToString())
                match1
            | (None, state) when state = input -> parser input
            | (None, state) -> raise (Error("No match found during OR and underlying state was modified")) 

    let getReply (current:Parser<'B>) (next:'B -> Parser<'A>)  (input : State): Reply<'A> =
        let match1 = current input
        match match1 with 
            | (Some(result), state) -> 
                let parser2 = next result
                parser2 state                    
            | (None, state) when state <> input -> 
                raise (Error("No match found and underlying state was modified")) 

            | (None, state) -> (None, state)

    let (>>=)  (current:Parser<'B>) (next:'B -> Parser<'A>)  : Parser<'A> = getReply current next                                   

    let (>>=?) (current:Parser<'B>) (next:'B -> Parser<'A>) : Parser<'A> = 
        fun state ->
            try
                getReply current next state
            with
                | e -> (None, state)
        
    let (|>>)  parser targetType : Parser<'T> = parser >>= fun value -> preturn (targetType value)

    let (|>>%) parser targetType : Parser<'T> = parser >>= fun _ -> preturn targetType

    let (>>.)  parser1 parser2 : Parser<'T> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn second

    let (.>>)  parser1 parser2 : Parser<'T> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn first

    let (.>>.) parser1 parser2 : Parser<'a * 'b> = 
        parser1 >>= fun first -> parser2 >>= fun second -> preturn (first, second)

    let (<|>) parser1 parser2 : Parser<'T> = getOptionReply parser1 parser2        

    let matcher stateChange eval target: Parser<'T> = 
        let p : Parser<'T> = 
            fun currentState -> 
                match eval target currentState with
                    | Some(result) -> Some(result), (stateChange currentState result)
                    | _ -> (None, currentState)
        p

    let many (parser : Parser<'T>) = 
        let p : Parser<'T list> = 
            fun state ->
                let rec many' parser (found, currentState) =                    
                    match parser currentState with
                        | (Some(m), nextState) ->        
                            System.Console.WriteLine("found {0}", m.ToString())
                            many' parser (m::found, nextState)
                        | _ -> 
                            if List.length found > 0 then
                                (Some(List.rev found), currentState)          
                            else
                                (None, currentState)
                                  
                many' parser ([], state)

        p

    let test input (parser:Parser<'T>) = 
        match parser input with
            | (Some(m), _) -> m
            | (None, _) -> raise (Error("No matches"))
