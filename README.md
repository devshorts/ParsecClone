ParsecClone
===========

This was an afternoon experiment to see if, without looking at the parsec or fparsec source, if I could mimic some of the parsec functionality. Included operators are

- `>>=` - combiner with function callback
- `>>=?` - combiner with function callback and backtracking
- `>>.` - use result of second combinator
- `.>>` - use result of first combinator
- `preturn` - return a value as a combinator
- `|>>` - pipe value into union or constructor
- `|>>%` - pipe to zero argument discriminated union
- `<|>` - first parser or second parser, as long as the or'd parsers don't modify the underlying state
- `many` - repeats a parser
- `matchStr` - matches a string if it starts with some text
- `match` - generic match on predicate and executes state modifier to return result

As it stands, this is just a string parser combinator. You can see the types here

```fsharp
type State = string

type Reply<'T> = 'T option * State

type Parser<'T> = State -> Reply<'T>
```

I chose this since it made for a good experiment. Included is a set of visual studio unit tests (for vs2012) that demonstrates the combinators.

The real power of the combinators come from this function:

```fsharp
getReply (current:Parser<'B>) (next:'B -> Parser<'A>)  (input : State): Reply<'A> 
```

Which takes a current parser, a function that takes the result of the current parser and returns a new parser, and the current state.  It applies the current state to the current parser, to get a result from it. Then, if the result yields a match, it executes the second parser with the new resulting state. Keep in mind that a parser can be a whole series of functions. However, executing this directly doesn't let us chain results. To chain a result you have to defer the call:

```fsharp
let (>>=)  (current:Parser<'B>) (next:'B -> Parser<'A>)  : Parser<'A> = getReply current next                                   
```

The `>>=` function returns a function that wants a state.  Since its just a function that takes a state and returns a `Reply`, we've effectively chained the combinators together.   
  

Once you can chain things, all the other functions are just wrappers and helpers leveraging the first combiner.