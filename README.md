ParsecClone
===========

This was an afternoon experiment to see if I could, without looking at the parsec or fparsec source, mimic some of the parsec functionality. 

In general, Combinators are a way to express complex parsing and chained actions in composable functions.  I've been playing with fparsec for the last month and that inspired me to try my hand at making combinators as well. Since I wanted to do it strictly for learning I decided to mimic the fparsec combinator syntax.

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
- `many` - repeats a parser
- `match` - generic match on predicate and executes state modifier to return result
- `matchStr` - matches a string if it starts with some text (uses match)
- `regexStr` - takes a regular expression and tests to see if the current state begins with a match (uses match)
- `anyOf` - takes a combinator and a list of strings and or's them all together with the `<|>` combinator

As it stands, this is just a string parser combinator. You can see the types here

```fsharp
type State = string

type Reply<'T> = 'T option * State

type Parser<'T> = State -> Reply<'T>
```

I chose to pin the state type (string) since it made for a good experiment. Included is a set of visual studio unit tests (for vs2012) that demonstrates the combinators.

Explanation
---

Combinators, to be useful, need a way to be combined and chained. But before we look at how to combine them, look at how to evaluate two already combined parsers.

```fsharp
getReply (current:Parser<'B>) (next:'B -> Parser<'A>)  (input : State): Reply<'A> 
```

This function takes an initial parser, a function that accepts the result of the current parser and returns a new parser, as well as the current state.  The idea is that `getReply` will apply the current state to the current parser, to get a result from it. Then, if this result yields a match (so its a `Some('a)`), it executes the second parser with the new resulting state. This gives us the result from the combination of both parsers. 

But, we need to be able to create new parsers.  Since a parser is a type of `State -> Reply<'T>` and we already have a function that can generate a `Reply<'T`> (above), then we need a way to create a function that takes a `State` and returns a `Reply<'T>`. Look closely though, we already happen to have that! If you curry the last argument off of `getReply` you now have a function that takes a `State` and returns a `Reply<'T>`.  Since its new signature is now `State -> Reply<'T>` that is equivalent to a combined parser!
 
Now we can tie in shortcut operators like FParsec has:

```fsharp
let (>>=)  (current:Parser<'B>) (next:'B -> Parser<'A>)  : Parser<'A> = getReply current next                                   
```

The `>>=` function returns a function that wants a state.  Since its just a function that takes a state and returns a `Reply`, we've effectively chained the combinators together.   
  

Once you can chain things, all the other functions are just wrappers and helpers leveraging the first combiner. Neat!