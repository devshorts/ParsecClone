ParsecClone
===========

This was an afternoon experiment (that ballooned a little) to see if I could, without looking at the parsec or fparsec source, mimic some of the parsec functionality. I give no guarantees to the correctness of this project, since its sole purpose was educational.

In general, Combinators are a way to express complex parsing and chained actions in composable functions.  I've been playing with fparsec for the last month and that inspired me to try my hand at making combinators as well. Since I wanted to do it strictly for learning I decided to mimic the fparsec combinator syntax.

One major difference between this and fparsec is that my string parsing is based on regex, not single character parsing. To me, this makes parsing a little easier since I struggled with the string parsing in fparsec.  Also it's kind of nice to not be an exact clone, because that's no fun.

Included operators are

- `>>=` - combiner with function callback

- `>>=?` - combiner with function callback and backtracking
- `>>.` - use result of second combinator
- `.>>` - use result of first combinator
- `preturn` - return a value as a combinator
- `pzero` - defines a zero (for use with folds and other starting states)
- `|>>` - pipe value into union or constructor
- `|>>%` - pipe to zero argument discriminated union
- `<|>` - first parser or second parser, as long as the or'd parsers don't modify the underlying state
- `many` - repeats a parser zero or more times
- `match` - generic match on predicate and executes state modifier to return result
- `matchStr` - matches a string if it starts with some text (uses match)
- `regexStr` - takes a regular expression and tests to see if the current state begins with a match (uses match)
- `anyOf` - takes a combinator and a list of strings and or's them all together with the `<|>` combinator
- `choice` - takes a list of parsers and or's them together with `<|>`
- `attempt` - if no match occurs or an exception happens, backtracks to the beginning of the state of the parser
- `takeTill` - takes a predicate and a parser and consumes until the predicate is true. Then backtracks one element
- `takeWhile` - same as take till except inverted predicate
- `manyN` - takes a number and tries to consume N parsers. If it doesn't consume exactly N it will fail
- `many1` - repeats a parser one or more times (fails if no match found)
- `lookahead` - returns a result and a new parser, but backtracks the state 
- `manyTill` - takes a parser and an end parser, and repeats the first parser zero or more times until the second parser succeeds
- `manyTill1` same as manyTill except fails on zero matches, so expects at least one or more
- `between` - takes a bookend parser, the parser, and another bookened parse and returns the value of the middle parser
- `manySatisfy` - alias for `takeWhile`
- `satisfy` - takes a predicate and a parser, applies the parser once and if the return result passes the predicate returns the result, otherwise backtracks.
- `opt` - takes a parser, applies the the state, if it returns a result returns a Some, otherwise returns a None. Up to you how you chain this. The `ws` parser uses this

This is both a string parser, and a binary parser, which is extensible to any parsing type (either on streams or primitives).  I've encapsulated the parsing state into the `IStreamP` interface which takes both the underlying state type, and what to return when it does a raw state consume. For example, on a `Stream` you want to consume `byte[]`. But on a `string` you want to consume other `string` types. In general, F#'s type inference system hides this nastiness away.

```fsharp
type State<'StateType, 'ConsumeType> = IStreamP<'StateType, 'ConsumeType>

type Reply<'Return, 'StateType, 'ConsumeType> = 'Return option * State<'StateType, 'ConsumeType>

type Parser<'Return, 'StateType, 'ConsumeType> = State<'StateType, 'ConsumeType> -> Reply<'Return, 'StateType, 'ConsumeType>
```

The state is generalized so we can plug in different stream sources (strings, streams, whatever).  For example, here is the state interface that all state consumers need to implement. The assumption is that consuming should always say how *much* to consume, which is why the consume function takes an integer.  If it can consume that much it will return consume type from the state type.

```fsharp
type IStreamP<'StateType, 'ConsumeType> =        
    abstract member state : 'StateType
    abstract member consume : IStreamP<'StateType, 'ConsumeType> -> int -> 'ConsumeType option * IStreamP<'StateType, 'ConsumeType>
    abstract member backtrack : unit -> unit
```

An unfortunate side effect of this is that I ran into a value restriction because of the generics, so every combinator needs to include a generic declaration like 

`let chars<'a> = //..whatever"` 
 
Included is a set of nunit tests that demonstrates the combinators (both string and binary).

Explanation
---

Combinators, to be useful, need a way to be combined and chained. But before we look at how to combine them, look at how to evaluate two already combined parsers.

```fsharp
getReply (current:Parser<'A, 'Y>) (next:'A -> Parser<'B, 'Y>)  (input : State<'Y>): Reply<'B, 'Y>  
```

This function takes an initial parser, a function that accepts the result of the current parser and returns a new parser, as well as the current state.  The idea is that `getReply` will apply the current state to the current parser, to get a result from it. Then, if this result yields a match (so its a `Some('a)`), it executes the second parser with the new resulting state. This gives us the result from the combination of both parsers. 

But, we need to be able to create new parsers.  Since a parser is a type of `State -> Reply<'T>` and we already have a function that can generate a `Reply<'T`> (above), then we need a way to create a function that takes a `State` and returns a `Reply<'T>`. Look closely though, we already happen to have that! If you curry the last argument off of `getReply` you now have a function that takes a `State` and returns a `Reply<'T>`.  Since its new signature is now `State -> Reply<'T>` that is equivalent to a combined parser!
 
Now we can tie in shortcut operators like FParsec has:

```fsharp
let (>>=)  (current:Parser<'A, 'Y>) (next:'A -> Parser<'B, 'Y>)  : Parser<'B, 'Y> = getReply current next                                                                      
```

The `>>=` function returns a function that wants a state.  Since its just a function that takes a state and returns a `Reply`, we've effectively chained the combinators together.   

Simple Example
---

Lets run through an example.  Let's make a parser:

```fsharp
let matchesName = matchStr "name"
```

Here, `matchStr` is a function that wants a state. We could explicity call it like this:

```fsharp
let state = "name is bob"
let result = matchStr "name" state
```

Here, result is a `Result option * State`, because the parser internally will have returned whether we found what we wanted or not, AND it returned the new state. The new state would be ` is bob` since `name` was consumed. 

Ok, fine, but what if we want to first match on name, and if that succeeds, then match on `is bob`?  Let's do this:

```fsharp
type Names =
	| Bob
	| Martin
	| Angela

let state = "name is bob"
let matchesName = matchStr "name"
let matchesIsBob = matchStr " is bob"
let combined = matchesName >>. matchesIsBob |>>% Bob
let result = combined state
```

If `matchesName` is a function that wants a state and returns a reply, and `matchesIsBob` is also a function that wants a state and returns a reply, what we need now is a combiner to take the first function and (if it succeeds) take its result and the new state and give it to the second function:

```fsharp
let (>>=)  firstParser secondParser  : Parser<'ReturnType, 'StateType> = 
	fun inputState ->
		let resultOfFirst = firstParser input
		    match resultOfFirst with 
		        | (Some(result), state) -> secondParser result state        
		        | (None, state) -> (None, state)                
```

So, see how `>>=` returns a new function which takes the first parser and the second parser, and returns a new function that applies the output of the first to the input of the second.  

The `|>>%` operator will take the result of the applied parsers and return the resulting union as a parser. When I say parser, just imagine a function that takes a state that returns a reply.  The reply type now is going to be `Names`. So we've lifted the type from just string into the union. Now we can build more complex structures and have things be strongly typed.
  
By leveraging F#'s currying, you can continue to build functions and functions and functions, until finally you seed the last function with the state you want to parse.  The rest, is just executing all the combined functions.

A more complicated example
---
Now, lets actually use my parsec clone. Below is a grammar that will parse csv files

```fsharp
namespace StringMatchers

open Combinator
open StringCombinator

module CsvSample = 
    
    let delimType = ","

    let(|DelimMatch|EscapedType|Other|) i = 
        if i = "\\" || i ="\"" then EscapedType
        else if i = delimType then DelimMatch
        else Other

    let delim<'a> = matchStr delimType

    let quote  = matchStr "\""

    let validNormalChars character = 
                            match character with
                                | EscapedType                                
                                | DelimMatch -> false
                                | Other -> not (isNewLine character)

    let inQuotesChars c = match c with                                 
                            | "\"" -> false
                            | _ -> true


    let unescape c = match c with
                     | "n" -> "\n"
                     | "r" -> "\r"
                     | "t" -> "\t"                     
                     | c   -> c


    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldChars

    let escapedChar<'a> = matchStr "\\" >>. (anyOf matchStr [delimType; "\"";"n";"r";"t"] |>> unescape)
    
    let normal<'a> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldChars

    let literal<'a> = between quote quoteStrings quote

    let csvElement = ws >>. (literal <|> normalAndEscaped)

    let listItem<'a> = delim >>. opt csvElement

    let elements<'a> = opt csvElement      >>= fun elem -> 
                       opt (many listItem) >>= 
                        function 
                        | Some(manyElem) -> preturn (elem::manyElem)
                        | None -> preturn (elem::[])
                        

    let lines<'a> = many (elements |> sepBy <| newline) .>> eof
```

If you've done the fparsec JSON tutorial this should look pretty similar.  The basic gist is that you want to allow both string literals within quotes, and regular escaped characters. So:

```
foo, bar, baz
```

Is a valid csv, but so is

```
foo\,,,bar,baz\"
```

And so is

```
"foo,",bar,baz
```
CSV's are annoying!

Conclusion
---

Hopefully you can see now that once you can chain things, all the other functions are just wrappers and helpers leveraging the first combiner. Neat!