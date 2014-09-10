ParsecClone 
===========

This a fparsec subset clone that works on generalized stream classes. This means you can use combinators on binary streams, strings, or any other custom stream classes you want. Included in the project is a sample CSV parser and a sample MP4 header binary parser. 


## Table of contents
- [Installation](#installation)
- [Target audience for documentation](#target-audience)
- [When to use and known limitations](#when-to-use-and-known-limitations)
- [Types and notation](#types-and-notation)
- [Generic operators](#generic-operators)
- [String operators](#string-operators)
- [Binary operators](#binary-operators)
- [Bit parsers](#bit-parsers)
- [Bit parsing order](#bit-parsing-ordering)
- [Computation Expression Syntax](#computation-expression-syntax)
- [A note on FParsec vs ParsecClone regarding strings](#a-note-on-fparsec-vs-parsecclone-regarding-strings)
- [Instantiating user states](#instantiating-user-states)
- [Dealing with value restrictions](#value-restrictions)
- [A CSV parser example (string)](#a-csv-parser)
- [An MP4 parser example (binary)](#binary-parser-example)
- [Bit parsing example](#binary-bit-parsing)
- [Improving binary performance](#improving-binary-performance)

## Installation

Install ParsecClone v1.1.1 via [NuGet](https://www.nuget.org/packages/ParsecClone/1.1.0)

```
Install-Package ParsecClone
```

This will install the ParsecClone F# library.  Also included is `ParsecClone.CombinatorCS` which contains an optimized byte array to structure mapper, which can greatly improve binary parsing performance.  It is recommended to reference both dll's even if you don't use structs.

Included in the main `ParsecClone.Combinator` dll are:

- The general operators are in `ParsecClone.CombinatorBase`
- The string handling in `ParsecClone.StringCombinator`
- And the binary operators in `ParsecClone.BinaryCombinator`



[[Top]](#table-of-contents) 

## Target Audience 

The documentation below is intended for people who are familiar with combinator libraries. If you are not familiar with [FParsec style combinators](http://www.quanttec.com/fparsec/) and notation, you may want to run through their [tutorials](http://www.quanttec.com/fparsec/tutorial.html) and explanations first.   

While the following documentation is not as robust as theirs, ParsecClone operators are very similar. Once you are familiar with FParsec operator and operator styles the following documentation should be enough to get you on your way.

[[Top]](#table-of-contents) 

## When to use and known limitations

ParsecClone is well suited for binary parsing which works on stream sources (memory streams, file streams, etc). Not only can you do byte level parsing, but also bit level parsing.  Performance of parsecClone is close to native. In my tests it was only 2x times slower than hand written c++.  Performance even exceeded C++ if you ran the parser multiple times (since the JIT had already run)!

ParseClone can also parse strings, but doesn't work on string streams. One of the reasons is that to use regular expressions you need to have unlimited lookahead to your stream. With a stream you'd end up having to read the whole stream in anyways!  Since FParsec works on streams, I chose to not duplicate that functionality. 

If you have strings you can buffer into memory, ParsecClone will work great (so smaller files that you can read all in one go). 

More importantly, ParsecClone is great for adding new stream sources to extend its capabilities. To do so just implement the `IStreamP` interface and hook into the `matcher` function in the base combinator library.

A few other caveats.  Currently ParsecClone's string parsing doesn't do any memoization, so you are stuck reparsing data.  However, by default the binary parser does memoize using a custom cache. You can disable this by passing in a `None` to the cache instantiator.

[[Top]](#table-of-contents)

## Types and notation

ParsecClone uses 3 main types for all of its combinators.

```fsharp
type State<'StateType, 'ConsumeType, 'UserState> = IStreamP<'StateType, 'ConsumeType, 'UserState>

type Reply<'Return, 'StateType, 'ConsumeType, 'UserState> = 'Return option * State<'StateType, 'ConsumeType, 'UserState>

type Parser<'Return, 'StateType, 'ConsumeType, 'UserState> = State<'StateType, 'ConsumeType, 'UserState> -> Reply<'Return, 'StateType, 'ConsumeType, 'UserState>
```

Since the types are kind of nasty, in the following operator examples I will use a shorthand notation of


```fsharp
Parser<'Return> implies Parser<'Return,_,_,_>
```

If other type information is needed in the signature I'll use the full parser type signature.

[[Top]](#table-of-contents)

## Generic Operators


Included operators are

```fsharp
val (>>=) : Parser<'a> -> ('a -> Parser<'b>) -> Parser<'b>
```
Combiner with function callback

----------

```fsharp
val (>>=?) : Parser<'a> -> ('a -> Parser<'b>) -> Parser<'b>
```
Combiner with function callback and backtracking

----------

```fsharp
val (>>.) : Parser<'a> -> Parser<'b> -> Parser<'b>
```
Use result of second combinator

----------

```fsharp
val (.>>) : Parser<'a> -> Parser<'b> -> Parser<'a>
```
Use result of first combinator

----------

```fsharp
val preturn: 'a -> Parser<'a>
```
Return a value as a combinator

----------

```fsharp
val pzero: unit -> Parser<'a>
```
Defines a zero (for use with folds and other starting states). Result is `(None, state`)

----------


```fsharp
val (|>>) : 'a -> ('a -> 'b) -> Parser<'b>
```

Pipe value into union or constructor

----------

```fsharp
val |>>%) : 'a -> Parser<'a>
```

Pipe to zero argument discriminated union

----------

```fsharp
val <|>) : Parser<'a> -> Parser<'b> -> Parser<'c>
```

Takes two parsers, and returns a new parser.  The result is either the result of the first parser (if it succeeds) or the result of the second parser, as long as the or'd parsers don't modify the underlying state.

----------

```fsharp
val .<?>>.) : Parser<'a> -> Parser<'a list> -> Parser<'a list>
```

Takes a single parser, and a list parser and applies both parers as options. 

If the first parser succeeds and the second parser fails, returns a list of the result of the first parser (`Some('a)::[]`). 

If the first parser succeeds and the second parser succeeds returns a cons list of both results (`Some('a)::Some('a) list`). This operator does not backtrack but will not fail if the first parser doesn't succeed (since its wrapped as an `opt`).

If the first parser fails, this parser fails.

----------

```fsharp
val .<<?>.) : Parser<'a list> -> Parser<'a> -> Parser<'a list>
```

The same as `.<?>>.` except with the arguments inverted. The list parser is first and the single parser is second. 

If the first parser fails, this parser fails.

----------

```fsharp
val (>>--): Parser<'a> -> (unit -> 'a) -> Parser<'a>
```

This operator lets you capture the actual invocation result of a parser.  For example, say you want to time how long a parser takes. You can create a time function like this:

```fsharp
let time identifier func =
	let start = System.DateTime.Now
    let value = func()
    printfn "%s Took %s" s ((System.DateTime.Now - start).ToString())
	value
```

And time an operator like

```fsharp
let newParser = parserImpl >>-- time "parserImpl"
```

Internally the right hand function is delayed and not executed till we actually call the parser:

```fsharp
let (>>--) parser wrapper = 
        fun state -> 
            wrapper (fun () -> parser state)
```     

----------

```fsharp
val (>>|.): Parser<'a> -> ('a -> 'b) -> Parser<'b>
```

Takes a parser and a transformer, applies the result of the parser to the transformer and returns a new parser that returns the transformed result.

----------
```fsharp
val many: Parser<'a> -> Parser<'a list>
```

Repeats a parser zero or more times, until the parser fails to match or the end of stream is encountered.

----------

```fsharp
val matcher: (State<_, 'ConsumeType, _>  -> 'a -> int option) -> 'a -> Parser<'ConsumeType>
```

Generic match on predicate and executes state modifier to return result

----------

```fsharp
val anyOf: ('a -> Parser<'a>) -> 'a list -> Parser<'a> 
```

Takes a function that maps the list into a bunch of parsers and or's each result together with the `<|>` combinator. For example: `anyOf matchStr ["f";"o";"i";"g";"h";"t";"e";"r";"s";" "]`

----------

```fsharp
val choice: Parser<'a> list -> Parser<'a>
```

Takes a list of parsers and or's them together with `<|>`

----------

```fsharp
val attempt: Parser<'a> -> Parser<'a>
```

If no match occurs or an exception happens, backtracks to the beginning of the state of the parser

----------

```fsharp
val takeTill: ('a -> bool) -> Parser<'a> -> Parser<'a list>
```

Takes a predicate and a parser and consumes until the predicate is true. Then backtracks one element

----------

```fsharp
val takeWhile: ('a -> bool) -> Parser<'a> -> Parser<'a list>
```

Takes a predicate and a parser and consumes until the predicate is false. Then backtracks one element

----------

```fsharp
val manyN: int -> Parser<'a> -> Parser<'a list>
```

Takes a number and tries to consume N parsers. If it doesn't consume exactly N it will fail. Aliased by `exactly`.

----------

```fsharp
val many1: Parser<'a> -> Parser<'a list>
```

Repeats a parser one or more times (fails if no match found)

----------

```fsharp
val lookahead: Parser<'a> -> Parser<'a>
```

Returns a result and a new parser, but backtracks the state 

----------

```fsharp
val manyTill: Parser<'a> -> Parser<'b> -> Parser<'a list>
```

Takes a parser and an end parser, and repeats the first parser zero or more times until the second parser succeeds

----------

```fsharp
val manyTill1: Parser<'a> -> Parser<'b> -> Parser<'a list>
```

Same as `manyTill` except fails on zero matches, so expects at least one or more

----------

```fsharp
val between: Parser<'a> -> Parser<'b> -> Parser<'c> -> Parser<'b>
```

Takes a bookend parser, the parser, and another bookened parse and returns the value of the middle parser

----------

```fsharp
val between2: Parser<'a> -> Parser<'b> -> Parser<'b>
```

Takes a bookend parser and the target parser, and applies the bookend parser twice to `between`. Usage could be for `parser |> between2 quote`

----------

```fsharp
val eof: Parser<unit>
```

Parser succeeds if the stream has nothing left to consume.  Fails otherwise.

----------

```fsharp
val manySatisfy: ('a -> bool) -> Parser<'a> -> Parser<'a list>
```

Alias for `takeWhile`

----------

```fsharp
val satisfy : ('a -> bool) -> Parser<'a> -> Paser<'a>
```

Takes a predicate and a parser, applies the parser once and if the return result passes the predicate returns the result, otherwise backtracks.

----------

```fsharp
val satisfyUserState : ('UserState -> bool) -> Parser<'a,_,_'UserState> -> Paser<'a>
```

Takes a predicate and a parser. It applies the parser and then calls the predicate with the new userstate. If the predicate succeeds, returns the result of the parser, otherwise it backtracks.

----------

```fsharp
val opt : Parser<'a> -> Parser<'a option>
```

Takes a parser, applies the the state, and returns a result option. Careful using this in the context of a 'many' since it you can get into infinite loops since you always "succeed"

----------

```fsharp
val createParserForwardedToRef: unit -> (Parser<'a>, ref Parser<'a>)
```

Returns a tuple of (parser, ref parser) to use for recursive calling parsers 

----------

```fsharp
val reproc elevator seed parser : ('result -> 'userState -> State<'newStateType,'newConsumeType,'userState>) -> 
									 Parser<'result, 'originalStateType, 'consumeType, 'userState> -> 
									 Parser<'b, 'newStateType, 'newConsumeType, 'userState> -> 
									 Parser<'b, 'originalStateType, 'consumeType, 'userSTate>
```

This functions lets you apply a parser to a buffered set of data. The buffered set of data acts as its own parser state. The seed is a parser on the original state and is used to create a new parse state (by the elevator). 

The second parser argument is the parser that will be applied to the new state.  The original state is advanced by the amount that the seed consumed. 

The return result is the return from the elevated parser, but the returned parser type continues to work on the underlying state.

----------

```fsharp
val getUserState : unit -> Parser<'UserState, 'StateType, 'ConsumeType, 'UserState>
```

Returns a parser whose result is the currently stored user state

----------

```fsharp
val setUserState : 'UserState -> Parser<unit>
```

Takes a value and sets the userstate with that value

----------
```fsharp
val statePosition: unit -> Parser<int64>
```

Returns the position of the state. Does not modify the stream.

----------

```fsharp
val parse: ParserCombinator
```

An instance of the `ParserCombinator` workflow builder, which allows you to do workflow based combinators (basically a direct port of FParsecs combinator).

-----------

[[Top]](#table-of-contents)

## String operators

One major difference between this and fparsec is that my string parsing is based on regex, not single character parsing. To me, this makes parsing a little easier since I struggled with the string parsing in fparsec.  Also it's kind of nice to not be an exact clone, because that's no fun.

String operators in the `StringP` module are:


----------

```fsharp
val matchStr: string -> Parser<string>
```

Matches a string if it starts with some text uses match


----------

```fsharp
val regexStr: string -> Parser<string>
```

Takes a regular expression and tests to see if the current state begins with a match uses match

----------

```fsharp
val any: Parser<string>
```

Parses the regex `.` from the stream.

----------

```fsharp
val anyBut: string -> Parser<string>
```

Takes a regular expression and returns a character that does not match the regex


----------

```fsharp
val char: Parser<string>
```

Parses the regex `[a-z]` from the stream.


----------

```fsharp
val chars: Parser<string>
```

Parses the regex `[a-z]+` from the stream.


----------

```fsharp
val digit: Parser<string>
```

Parses the regex `[0-9]` from the stream.


----------

```fsharp
val digits: Parser<string>
```

Parses the regex `[0-9]+` from the stream.


----------

```fsharp
val newline: Parser<string>
```

Matches `\r\n` or `\n:` or `\r`


----------

```fsharp
val whitespace: Parser<string>
```

Parses the regex `\s` from the stream.


----------

```fsharp
val whitespaces: Parser<string>
```

Parses the regex `\s+` from the stream.


----------

```fsharp
val space: Parser<string>
```

Parses the regex `" "` from the stream.


----------

```fsharp
val spaces: Parser<string>
```

Parses the regex `" "+` from the stream.


----------

```fsharp
val tab: Parser<string>
```

Parses the regex `\t` from the stream.


----------

```fsharp
val tabs: Parser<string>
```

Parses the regex `\t+` from the stream.


----------

```fsharp
val ws: Parser<string>
```

optional whitespace parser. Always succeeds, if it consumes actual whitespace the resulting string will not be an empty string. If it fails, it will return an empty string.


----------

```fsharp
val foldStrings: string list -> Parser<string>
```

takes a string list and preturns a concatenated version of those strings `string list -> parser<string>` 

----------
```fsharp
val makeStringStream : String -> StringStreamP<unit>
```

Utility function to create a stream from a string. Use this if you don't need to create any user state.

----------
```fsharp
val isNewLine : String -> bool
```

Returns true if the string is `\r\n`, `\n`, or `\r`.

[[Top]](#table-of-contents)

## Binary operators

To use a binary parser, you need to instantiate the `BinParser` class, which is the container for these operators. They are not imported into the global space. The reason being that you can pass an endianess converter to it. The endianess converter is run against all number converters, but not anything else.   

```fsharp
let bp = new BinParser<_>(Array.rev)
```

The BinParser takes a generic argument representing the userstate of the stream.  In general, just declare it as an unknown parameter and the type inference system will figure it out for you.

Binary operators of the `BinParser` class in the `BinaryParser` module are:

----------
```fsharp
val makeBinStream: Stream -> BinStream<unit>
```

Helper function to take a stream and create a `BinStream` instance for use with the binary combinators. Use this if you don't need any user state.

----------

```fsharp
val byteN: int -> : Parser<byte[]>
```

Takes an integer N and consumes a byte array of that size


----------

```fsharp
val byte1: Parser<byte>
```

Returns one byte


----------

```fsharp
val byte2: Parser<byte[]>
```

Returns two bytes


----------

```fsharp
val byte3: Parser<byte[]>
```

Returns three bytes


----------

```fsharp
val byte4: Parser<byte[]>
```

Returns four bytes


----------

```fsharp
val intB: Parser<int>
```

Returns the byte value as a signed integer


----------

```fsharp
val int16: Parser<int16>
```

Parses 2 bytes and returns a signed 16 bit integer


----------

```fsharp
val int32: Parser<int32>
```

Parses 4 bytes and returns a signed 32 bit integer


----------

```fsharp
val int64: Parser<int64>
```

Parses 8 bytes and returns a signed 64 bit integer


----------

```fsharp
val uintB: Parser<uint>
```

Returns the byte value as an unsigned integer


----------

```fsharp
val uint16: Parser<uint16>
```

Parses 2 bytes and returns an unsigned 16 bit integer


----------

```fsharp
val uint32: Parser<uint32>
```

Parses 4 bytes and returns an unsigned 32 bit integer


----------

```fsharp
val uint64: Parser<uint64>
```

Parses 8 bytes and returns an unsigned 64 bit integer


----------

```fsharp
val skip: int -> Parser<bool>
```

Skips N bytes in the stream by seeking. Returns true if succeeded.


----------

```fsharp
val skiptoEnd: Parser<unit>
```

Skips to the end of the stream


----------

```fsharp
val shiftL: uint32 -> (uint32 -> Parser<uint32>)
```

Shifts left N bits


----------

```fsharp
val shiftR: uint32 -> (uint32 -> Parser<uint32>)
```

Shifts right N bits


----------

```fsharp
val floatP: Parser<float>
```

Parses a 4 byte float


----------

```fsharp
val matchBytes: byte[] -> Parser<byte[]>
```

Parses the exact byte sequence (as byte array). Result is the byte sequence you expected. Fails if the byte sequence is not found at the start of the stream.


----------

```fsharp
val byteToUInt : byte -> uint
```

Takes one byte, and converts to uint32


----------

```fsharp
val toUInt16 : byte[] -> uint16
```

Takes a 2 byte array, applies endianess converter, and converts to uint 16


----------

```fsharp
val toUInt24 : byte[] -> uint32
```

Takes a 3 byte array, applies endianess converter, and converts to uint 32


----------

```fsharp
val toUInt32 : byte[] -> uint32
```

Takes a 4 byte array, applies endianess converter, and converts to uint 32


----------

```fsharp
val toUInt64 : byte[] -> uint64
```

Takes a 8 byte array, applies endianess converter, and converts to uint 64


----------

```fsharp
val byteToInt : byte -> int
```

Takes one byte and converts to int32


----------

```fsharp
val toInt16 : byte[] -> int16
```

Takes a 2 byte array, applies endianess converter, and converts to int 16


----------

```fsharp
val toInt24 : byte[] -> int32
```

Takes a 3 byte array, applies endianess converter, and converts to int 32


----------

```fsharp
val toInt32 : byte[] -> int32
```

Takes a 4 byte array, applies endianess converter, and converts to int 32


----------

```fsharp
val toInt64 : byte[] -> int64
```

Takes a 8 byte array, applies endianess converter, and converts to int 64

----------

```fsharp
val parseStruct<'T, 'UserState>: bool -> int -> BinParser<'UserState> -> Parser<'T list>
```

This method is not defined on the `BinParser` object, but is auto included with the `BinaryCombinator` namespace. `'T` should be the struct type you want to parse.  Internally this will read `sizeof 'T * numEntries` bytes and marshal the bytes directly into the struct. This can be significantly faster than parsing each entry independently.  The bool, if true, says to use network order (big endian). In this case it will read the bytes from end to begin. Your struct should be ordered backwards for this to work properly (so first field last, last field first).  If the bool is false, it will be little endian.  The int parameter says the number of structs to parse. 

----------

```fsharp
val defineStructParserLE<'T>: int -> BinParser<unit> -> Parser<'T list>
```

Defines a little endian struct parser. This method is not defined on the `BinParser` object, but is auto included with the `BinaryCombinator` namespace.  Helper function to define a struct parser with no required user state. 

----------

```fsharp
val defineStructParserBE<'T>: int -> BinParser<unit> -> Parser<'T list>
```

Defines a big endian struct parser. This method is not defined on the `BinParser` object, but is auto included with the `BinaryCombinator` namespace.  Helper function to define a struct parser with no required user state.  
 
----------
[[Top]](#table-of-contents)

## Bit Parsers

Also included in the binary parser are bit level parsers. These parsers need to work on a "seeded" byte stream. For example, you need to read in a 2 byte block, and then do bit level parsing on the 2 byte block.  The byte stream will be advanced by 2 bytes, but you can work on the "seeded" (or cached) version of the stream with new parser types, by lifting the parser stream to a new stream type.  

The bit type that is returned looks like this

```fsharp
type Bit = 
    | One
    | Zero
```


Operators that make this possible include:

----------

```fsharp
val makeBitP: Parser<byte[]> -> Parser<'ReturnType> 
```

takes a seed parser (to provide the underlying byte array to use as the parser set) and a bit level parser and applies the bit level parser to the seed.  Bit parsers are complicated because the smallest thing you can read off the stream is a byte, so you have to work in memory on your byte stream.    


----------

```fsharp
val bitsN: int -> Parser<Bit[]>
```

Takes an integer N and returns an array of `Bit` union types (`Zero` or `One`)

----------

```fsharp
val bitsToInt: Bit [] -> int
```

Takes a bit array and converts it to an `int`


----------

```fsharp
val bitN : int -> Parser<Bit>
```

Takes an integer N and returns back the bit value at position N 


----------

```fsharp
val bit1 : Parser<Bit>
```

Returns the value of the first bit (zero or one)


----------

```fsharp
val bit2 : Parser<Bit>
```

Returns the value of the second bit (zero or one)


----------

```fsharp
val bit3 : Parser<Bit>
```

Returns the value of the third bit (zero or one)


----------

```fsharp
val bit4 : Parser<Bit>
```

Returns the value of the fourth bit (zero or one)


----------

```fsharp
val bit5 : Parser<Bit>
```

Returns the value of the fifth bit (zero or one)


----------

```fsharp
val bit6 : Parser<Bit>
```

Returns the value of the sixth bit (zero or one)


----------

```fsharp
val bit7 : Parser<Bit> 
```

Returns the value of the seventh bit (zero or one)


----------

```fsharp
val bit8 : Parser<Bit>
```

Returns the value of the eight bit (zero or one)

[[Top]](#table-of-contents)

## Bit parsing ordering

Bit parsing works left to right and doesn't get run through the endianness converter.  Here is the layout of what is meant by bit 1 through bit 8, 

```
0xF = 0b 1 1 1 1 1 1 1 1
  bit#   1 2 3 4 5 6 7 8
```

If you need to extend the bit parsing, there is a `BitStream` class that handles the bit handling from a byte array

[[Top]](#table-of-contents)

## Computation Expression Syntax

Just like in FParsec, ParsecClone supports workflow syntax:

```fsharp
[<Test>]
let testExpression() = 
    let state = makeStringStream "this is a test"

    let parser = parse {
        let! _ = matchStr "this"
        let! _ = ws
        let! _ = matchStr "is a"
        let! _ = ws
        return! matchStr "test"
    }

    let result = test state parser

    result |> should equal "test"
```

[[Top]](#table-of-contents)

## A note on FParsec vs ParsecClone regarding strings

One thing I really wanted to implement that Fparsec didn't have was regular expression support for strings.  Just to demonstrate what you need to do to parse a string given by the grammar

```
<f> 		 := "f"
<oos> 		 := "o" | "o"<oos>
<fighter> 	 := "fighter"
<foofighter> := <f><oos><fighter>
```

Basically the word `foofighter` with at least one or more `o`'s. Here is an example in fparsec

```
let fooString = pstring "f" >>= fun f ->
                many1 (pstring "o") >>= fun os ->
                pstring "fighter" >>= fun fighter ->
                preturn (f + (List.reduce (+) os) + fighter)

test fooString "foofighter" |> should equal "foofighter"
```

Here is an example in ParsecClone

```
let foo = regexStr "fo+fighter"

let state = makeStringStream "foofighter"

test state foo |> should equal "foofighter"
```

Just different flavors.  You can do the fparsec way in ParsecClone as well.  Arguably, I'd say that FParsec is more correct here since you are forced to implement the grammar without cheating with regex, but regex does make the problem succinct.

[[Top]](#table-of-contents)

## Instantiating User States


One thing that can happen is you need to track context sensitive information during your parsing. This is where the user state comes into play. For the simple cases, `makeStringStream` and `makeBinStream` create state sources that have no user state (`unit`).  To create a stream source with a custom user state type do the following:

```fsharp
type VideoState = { IsAudio: bool; StateStart: int64 }

let makeBinStreamState (stream:Stream) =
	new BinStream<VideoState>(stream, { IsAudio = false; StateStart = (int64)0 }, BinStreams.createCache())
```

In this scenario I am creating a user state of the record `VideoState` and seeding the `BinStream` with a default value. The user state is mutable, so you can pass it whatever you want.  I'm also creating the default binary cache to use for memoization.  If you don't want to use memoization (maybe your data source is huge and you would rather seek around in the stream), then pass in `None`. The cache is an option type.

[[Top]](#table-of-contents)

## Value Restrictions


Just like with FParsec, you can run into an F# value restriction. This is due to un-inferrable generics that are used by the parser types. There are a lot of generic types (more than FParsec, since this is more customizable).  The same rules apply here as with FParsec. If the parser gets used in some context where the final user state gets evaluated (for example by actually using the parser in a `test` function), OR, by directly qualifying the parser.

The mp4 sample does this, since it defines parsers in one assembly but uses them from another. Notice the trick here:

```fsharp
type VideoParser<'Return> = Parser<'Return, System.IO.Stream, byte[], VideoState>
```

This creates a generic type of `VideoParser` who's `'Return` type is unknown, but we know that it takes a `Stream`, it will consume `byte[]`, and the user state should be `VideoState`.  Then declare parsers return type as:

```fsharp
let video : VideoParser<_> = many (choice[  attempt ftyp; 
                                            moov; 
                                            mdat; 
                                            free;]) .>> eof
```

The other parsers don't need to be marked as `VideoParser` since they all get used from `video`.  If you have errors, pin the types. 

## A CSV Parser

Lets actually use my parsec clone. Below is a grammar that will parse csv files

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

    let validNormalChars = function
                            | EscapedType                                
                            | DelimMatch -> false
                            | rest -> not (isNewLine rest)

    let inQuotesChars  = function                                 
                            | "\"" -> false
                            | _ -> true

    let unescape = function
                     | "n" -> "\n"
                     | "r" -> "\r"
                     | "t" -> "\t"                     
                     | c   -> c

    let quoteStrings = (many (satisfy (inQuotesChars) any)) >>= foldStrings

    let escapedChar<'a> = matchStr "\\" >>. (anyOf matchStr [delimType; "\"";"n";"r";"t"] |>> unescape)
    
    let normal<'a> = satisfy validNormalChars any 

    let normalAndEscaped = many (normal <|> escapedChar) >>= foldStrings
    
    let literal<'a> = quoteStrings |> between2 quote

    let csvElement = many (literal <|> normalAndEscaped) >>= foldStrings

    let listItem<'a> = delim >>. ws >>. opt csvElement

    let elements<'a> = csvElement .<?>>. many listItem

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

All these cases work with the sample CSV parser

Here is an example of how to use the csv parser

```fsharp
[<Test>]
let testAll() = 
    let csv = makeStringStream @"This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"""

    let result = test csv lines

    List.length result |> should equal 11
```

[[Top]](#table-of-contents)

## Binary Parser Example

As another example, this time of the binary parser, I wrote a small [mp4 video file header parser](https://github.com/devshorts/ParsecClone/blob/master/Samples/Mp4Matcher/Mp4Sample.fs).  MP4 can be pretty complicated, so I didn't do the entire full spec, but you should be able to get the idea of how to use the binary parser.  

For a detailed explanation and walkthrough check the [sample readme](https://github.com/devshorts/ParsecClone/blob/master/Samples/Mp4Matcher)

[[Top]](#table-of-contents)

## Binary bit parsing


Parsing bits requires you to work on a pre-seeded byte stream. This is achieved by calling the `makeBitP` parser which reads a certain number of bytes from the byte stream, and then elevates the stream into a bit stream. The returned result from the `makeBitP` parser is the return result from the bit parsers.  Only bit parsers can be used in the bit parsing stream, byte parsers won't work.

Here is a simple example: 

```fsharp
[<Test>]
let bitParserTest() = 
    let bytes = [|0xF0;0x01|] |> Array.map byte

    let parserStream = makeBinStream <| new MemoryStream(bytes)   

    let bitToBool = bp.bitsN 4 

    let bitP = bp.makeBitP (byteN 1) bitToBool

    let result = test parserStream (bitP .>> bp.byte1 .>> eof)
    
    result |> should equal 15
```

We create a single byte seed to use for the bit parsing, and then read 4 bits from the read byte (the other 4 bits are ignored).  The underlying source stream was advanced by 1 byte, so I read the next byte to discard it, and then check for eof for completeness.  The final tests makes sure that the 4 bits we read were all ones, validating that the value is 15 (0b1111). 

Here is another example that applies the combinator `many` to the bitParser.  This example parses each bit and returns the bit value for a byte, and is applied to an array of 10 bytes

```fsharp
[<Test>]
let testApplyManyBits() = 
    let bytes = Array.init 10 (fun i -> byte(0x01))

    let parserStream = makeBinStream <| new MemoryStream(bytes)   
    
    let selectAllBits = bp.bit1 >>= fun one ->
                        bp.bit1 >>= fun two ->
                        bp.bit1 >>= fun three ->
                        bp.bit1 >>= fun four ->
                        bp.bit1 >>= fun five ->
                        bp.bit1 >>= fun six ->
                        bp.bit1 >>= fun seven ->
                        bp.bit1 >>= fun eight ->
                        preturn [|one;two;three;four;five;six;seven;eight|]
                        
    let bitP = bp.makeBitP (byteN 1) selectAllBits

    let result = test parserStream (many bitP .>> eof)
    
    let target = [0..Array.length bytes - 1] |> List.map (fun _ -> bytesToBits <| bytes.[0..0]) 

    result |> should equal target 
```

[[Top]](#table-of-contents)

## Improving Binary Performance

One easy win is to wrap your input stream with a `BufferedStream`. Check the included unit tests for an example.

The other big bottleneck in combinator parsing is when you need to parse the same item many times (maybe thousands, or hundreds of thousands of times). If you have to parse the same type many times in a row you should consider using a struct to hold your data type and leveraging the struct parsing functionality of ParsecClone.

Using the struct parsing requires a little bit of setup.  Let me demonstrate using the sample mp4 parser.

Due to use of generics, we need to pin the `UserState` for the struct parser.

```fsharp
/// <summary>
/// Creates a network order binary parser
/// </summary>
let bp = new BinParser<_>(Array.rev)

let pStructs<'T> bytes : VideoParser<_> = parseStruct<'T, VideoState> true bytes bp
```

The `parseStruct` function comes defined in the `BinParsers` module which is auto included with `ParsecClone.BinaryCombinator`.  The function takes a generic type `'T` which should be the struct type you want to parse, a boolean representing whether the bytes it reads are in network order (big endian) or not.  This is important because for some binary structures, the files are written big endian vs the .NET normal of little endian.  For big endian files, the bulk array is reversed, and read *backwards* during struct parsing. This means you should order your structures backwards as well, and then everything will map.

The other arguments to the struct parser include a byte array representing the entire byte chunk to read, and a reference to the binary parser instance.  

As an example, let's say we have the following struct:

```fsharp
[<Struct>]
type TimeToSampleEntry = 
    struct
        val SampleCount: uint32; 
        val SampleDuration: uint32 
    end
```

Which is contained in the following record:

```fsharp
type Stts = {
    Atom: AtomBase
    VersionAndFlags: VersionAndFlags
    NumberOfEntries: uint32
    SampleTimes: TimeToSampleEntry list
}
```

There could potentially be hundreds of thousands of the `TimeToSampleEntry` elements, so it's good for us to batch this processing instead of reading 8 bytes at a time.

To parse the struct you can do something like the following:

```fsharp
let stts : VideoParser<_> = 
    atom "stts" >>= fun id ->
    versionAndFlags     >>= fun vFlags ->
    bp.uint32           >>= fun numEntries ->
    pStructs<TimeToSampleEntry> (int numEntries) >>= fun samples ->
    preturn {
        Atom = id
        VersionAndFlags = vFlags
        NumberOfEntries = numEntries
        SampleTimes = samples
    } |>> STTS
```

Notice how `pStructs` takes the type of the struct as a generic as well as the number of structs to create.  Internally the struct parser will read `sizeof typeof('T) * count` bytes and marshal the raw byte array into the structure.  

In general, don't optimize prematurely.  The nice thing about records and structs is that it's trivial to change a record to a struct. Updating your parser requires only a line change (instead of a `manyN (int numEntries) parser` you replace it with the `pStructs` parser).


[[Top]](#table-of-contents)
