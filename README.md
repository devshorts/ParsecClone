ParsecClone
===========

This a fparsec subset clone that works on generalized stream classes. This means you can use combinators on binary streams, strings, or any other custom stream classes you want. I wanted to seperate out the combinator functionality from the stream. The project is reasonably complete, and I have a sample CSV parser and a sample MP4 header binary parser included in the source.

In general, combinators are a way to express complex parsing and chained actions in composable functions.  I've been playing with fparsec for the last month and that inspired me to try my hand at making combinators as well.I decided to mimic the fparsec combinator syntax since I really liked that format.

Installation
---
Install ParsecClone via [NuGet](https://www.nuget.org/packages/ParsecClone/0.1.0)

```
Install-Package ParsecClone
```

The general operators are in `ParsecClone.CombinatorBase`.

String handling is in `ParsecClone.StringCombinator`.

Binary handling is in `ParsecClone.BinaryCombinator`.

When to use and known limitations
---
ParsecClone is well suited for binary parsing which works on stream sources (memory streams, file streams, etc). Not only can you do byte level parsing, but also bit level parsing.

ParseClone can also parse strings, but doesn't work on string streams. One of the reasons is that to use regular expressions you need to have unlimited lookahead to your stream. With a stream you'd end up having to read the whole stream in anyways!  Since FParsec works on streams, I chose to not duplicate that functionality. 

If you have strings you can buffer into memory, ParsecClone will work great (so smaller files that you can read all in one go). 

More importantly, ParsecClone is great for adding new stream sources to extend its capabilities. Granted this requires a rebuild of the project, but its almost trivial to add in new consumable streams.  If you have a request for a stream source please open an issue for me to track.

A few other caveats.  Currently ParsecClone doesn't do any memoization, so you are stuck reparsing data.  

Generic Operators
----

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
- `.<?>>.` - takes a parser of `T` and a parser of `T list` and applies both parers as options. if the first parser succeeds and the second parser fails, returns a list of the result of the first parser (`Some(T)::[]`), if the first parser succeeds and the second parser succeeds returns a cons list of both results (`Some(T)::Some(T) list`). This operator does not backtrack but will not fail if the first parser doesn't succeed (since its wrapped as an `opt`).
- `.<<?>.` - basically the same as `.<?>>.` except with the arguments inverted. The list parser is first and the single parser is second. Returns a list of `T`.
- `>>..` - takes a parser and a processor function.  Applies the processor function to the result of the the parser. Alias for `parser1 >>= fun first -> applier first `. The applier should use preturn. An example applier is used in the binary parser to shift bits: `shiftL n = fun (b : uint32)  -> preturn (b <<< n)`. See the mp4 example below to see how it can be used in conjunction with a regular parser.
- `many` - repeats a parser zero or more times
- `match` - generic match on predicate and executes state modifier to return result
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
- `between2` - takes a bookend parser and the target parser, and applies the bookend parser twice to `between`. Usage could be for `parser |> between2 quote`
- `manySatisfy` - alias for `takeWhile`
- `satisfy` - takes a predicate and a parser, applies the parser once and if the return result passes the predicate returns the result, otherwise backtracks.
- `opt` - takes a parser, applies the the state, and returns a result option. Careful using this in the context of a `many` since it you can get into infinite loops since you always "succeed"
- `createParserForwardedToRef` - returns a tuple of (parser, ref parser) to use for recursive calling parsers 
- `reproc elevator seed parser` - This functions lets you apply a parser to a buffered set of data. The buffered set of data acts as its own parser state. The seed is a parser on the original state and is used to create a new parse state (by the elevator). The elevators signature is `'a -> IStreamP` where `'a` is the result type of the seed.   The second parser argument is the parser that will be applied to the new state.  The original state is advanced by the amount that the seed consumed.

String operators
----

One major difference between this and fparsec is that my string parsing is based on regex, not single character parsing. To me, this makes parsing a little easier since I struggled with the string parsing in fparsec.  Also it's kind of nice to not be an exact clone, because that's no fun.

String operators in the `StringP` module are:

- `matchStr` - matches a string if it starts with some text (uses match)
- `regexStr` - takes a regular expression and tests to see if the current state begins with a match (uses match)
- `anyBut` - takes a regular expression and returns a character that does not match the regex
- `char` - [a-z] character
- `chars` - [a-z]+ characters
- `digit` - [0-9] 
- `digits` - [0-9]+
- `newline` - matches `\r\n` or `\n` or `\r`
- `whitespace` - \s
- `whitespaces` - \s+
- `space` - " "
- `spaces` - " "+
- `tab` - \t
- `tabs` - \t+
- `ws` - optional whitespace parser
- `foldStrings` - takes a string list and preturns a concatenated version of those strings (`string list -> parser<string>`) 

Binary operators
----

To use a binary parser, you need to instantiate the `BinParser` class, which is the container for these operators. They are not imported into the global space. The reason being that you can pass an endianess converter to it. The endianess converter is run against all number converters, but not anything else.   

```fsharp
let bp = new BinParser(Array.rev)
```

Binary operators of the `BinParser` class in the `BinaryParser` module are:

- `byteN` - takes an integer N and consumes a byte array of that size
- `byte1` - returns one byte
- `byte2` - returns two bytes
- `byte3` - returns three bytes
- `byte4` - returns four bytes
- `intB` - returns the byte value as a signed integer
- `int16` - parses 2 bytes and returns a signed 16 bit integer
- `int32` - parses 4 bytes and returns a signed 32 bit integer
- `int64` - parses 8 bytes and returns a signed 64 bit integer
- `uintB` - returns the byte value as an unsigned integer
- `uint16` - parses 2 bytes and returns an unsigned 16 bit integer
- `uint32` - parses 4 bytes and returns an unsigned 32 bit integer
- `uint64` - parses 8 bytes and returns an unsigned 64 bit integer
- `skip` - skips N bytes in the stream by seeking
- `skiptoEnd` - skips to the end of the stream
- `shiftL` - shifts left N bits
- `shiftR` - shifts right N bits
- `floatP` - parses a 4 byte float
- `matchBytes` - parses the exact byte sequence (as byte array)
- `byteToUInt` - takes one byte, and converts to uint32
- `toUInt16` - takes a 2 byte array, applies endianess converter, and converts to uint 16
- `toUInt24` - takes a 3 byte array, applies endianess converter, and converts to uint 32
- `toUInt32` - takes a 4 byte array, applies endianess converter, and converts to uint 32
- `toUInt64` - takes a 8 byte array, applies endianess converter, and converts to uint 64
- `byteToInt` - takes one byte and converts to int32
- `toInt16` - takes a 2 byte array, applies endianess converter, and converts to int 16
- `toInt24` - takes a 3 byte array, applies endianess converter, and converts to int 32
- `toInt32` - takes a 4 byte array, applies endianess converter, and converts to int 32
- `toInt64` - takes a 8 byte array, applies endianess converter, and converts to int 64

Also included in the binary parser are bit level parsers. These parsers need to work on a "seeded" byte stream. For example, you need to read in a 2 byte block, and then do bit level parsing on the 2 byte block.  The byte stream will be advanced by 2 bytes, but you can work on the "seeded" (or cached) version of the stream with new parser types, by lifting the parser stream to a new stream type.  Operators that make this possible include:

- `makeBitP` - takes a seed parser (to provide the underlying byte array to use as the parser set) and a bit level parser and applies the bit level parser to the seed.  Bit parsers are complicated because the smallest thing you can read off the stream is a byte, so you have to work in memory on your byte stream.    
- `bitsN` - takes an integer N and returns a list of `Bit` union types (`Zero` or `One`)
- `bitsToInt` - takes a bit list and converts it to an `int`
- `bitN` - takes an integer N and returns back the bit value at position N 
- `bit1` - returns the value of the first bit (zero or one)
- `bit2` - returns the value of the second bit (zero or one)
- `bit3` - returns the value of the third bit (zero or one)
- `bit4` - returns the value of the fourth bit (zero or one)
- `bit5` - returns the value of the fifth bit (zero or one)
- `bit6` - returns the value of the sixth bit (zero or one)
- `bit7` - returns the value of the seventh bit (zero or one)
- `bit8` - returns the value of the eight bit (zero or one)

Bit parsing works left to right and doesn't get run through the endianness converter.  Here is the layout of what is meant by bit 1 through bit 8, 

```
0xF = 0b 1 1 1 1 1 1 1 1
  bit#   1 2 3 4 5 6 7 8
```

If you need to extend the bit parsing, there is a `BitStream` class that handles the bit handling from a byte array

A note on FParsec vs ParsecClone regarding strings
---
One thing I really wanted to implement that Fparsec didn't have regular expressions support for strings.  Just to demonstrate what you need to do to parse a string given by the grammar

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

let state = new StringStreamP("foofighter")

test state foo |> should equal "foofighter"
```

Just different flavors.  You can do the fparsec way in ParsecClone as well.  Arguably, I'd say that FParsec is more correct here since you are forced to implement the grammar without cheating with regex, but regex does make the problem succinct.

A CSV Parser
---
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
    let t = @"This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words"", This is some text! whoo ha, ""words""
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

    let csv = new StringStreamP(t)

    let result = test csv lines

    List.length result |> should equal 11
```

Binary Parser Example
---
As another example, this time of the binary parser, I wrote a small mp4 video file header parser.  MP4 can be pretty complicated, so I didn't do the entire full spec, but you should be able to get the idea of how to use the binary parser.

One limitation is there isn't a way to do bit level parsing, since the stream gives you things byte by byte. 

The high level overview looks like this:

```fsharp
namespace Mp4Matcher

open Combinator
open BinaryCombinator
open System.Text
open System

[<AutoOpen>]
module Mp4P = 
    
    let stbl<'a> = 
        basicAtom "stbl" >>= fun id ->        
        many (stts <|> stsd <|> stsz <|> stsc <|> stco <|> stss) |>> STBL

    let vOrSmhd<'a> = vmhd <|> smhd

    let minf<'a> = 
        basicAtom "minf" >>= fun id ->       
        many (vOrSmhd <|> dinf <|> stbl) |>> MINF

    let mdia<'a> = 
        basicAtom "mdia" >>= fun id ->        
        many (mdhd <|> hdlr <|> minf) |>> MDIA

    let trak<'a> = 
        basicAtom "trak" >>= fun id ->        
        many (tkhd <|> mdia) |>> TRAK

    let mdat<'a> = 
        basicAtom "mdat" >>= fun id ->
        if (int)id.Size = 0 then 
            bp.skipToEnd  >>. preturn id |>> MDAT
        else
            bp.skip ((int)id.Size-8) >>= fun _ ->
            preturn id |>> MDAT

    let moov<'a> = basicAtom "moov" >>. many (mvhd <|> iods <|> trak) |>> MOOV
    
    let video<'a> = many (choice[attempt ftyp; moov; mdat]) .>> eof
                        
```

Where leaf nodes look sometihng like this:

```fsharp
let mvhd<'a> = 
    basicAtom "mvhd"            >>= fun id -> 
    versionAndFlags             >>= fun vFlags ->
    date                        >>= fun creationTime ->
    date                        >>= fun modificationTime ->
    bp.uint32                   >>= fun timeScale ->
    bp.uint32                   >>= fun duration ->
    bp.uint32                   >>= fun rate ->
    bp.uint16                   >>= fun volume ->
    bp.skip 70                  >>= fun _ -> 
    bp.uint32                   >>= fun nextTrackId ->
    preturn {
        Atom = id
        VersionAndFlags = vFlags
        CreationTime = creationTime
        ModificationTime = modificationTime
        TimeScale = timeScale
    } |>> MVHD


let tkhd<'a> = 
    basicAtom "tkhd" >>= fun id ->
    versionAndFlags >>= fun vFlags ->
    date >>= fun creationTime ->
    date >>= fun modificationTime ->
    bp.uint32 >>= fun trackId ->
    bp.uint32 >>= fun reserved ->
    bp.uint32 >>= fun duration ->        
    bp.uint32 >>= fun layer ->
    bp.uint16 >>= fun alteranteGroup ->
    bp.uint16 >>= fun volume ->
    bp.byteN 8 >>= fun reserved ->
    manyN 9 bp.floatP >>= fun matrix ->
    bp.uint32 >>.. bp.shiftR 16 >>= fun width ->
    bp.uint32  >>.. bp.shiftR 16 >>= fun height ->
    preturn {
        Atom  = id
        VersionAndFlags = vFlags
        CreationTime  = creationTime
        ModificationTime  = modificationTime
        TrackId = trackId
        Duration = duration
        Layer = layer
        AlternateGroup = alteranteGroup
        Volume = volume
        Height = width
        Width = height
    } |>> TKHD
```

Binary bit parsing
----

Bit parsing is new, and I don't have a big example yet, but here is a sample from a unit test:

```fsharp
[<Test>]
let bitParserTest() = 
    let bytes = [|0xF0;0x01|] |> Array.map byte

    let parserStream = new BinStream(new MemoryStream(bytes))   

    let bitToBool = bp.bitsN 4 

    let bitP = bp.makeBitP (byteN 1) bitToBool

    let result = test parserStream (bitP .>> bp.byte1 .>> eof)
    
    result |> should equal 15
```

We create a single byte seed to use for the bit parsing, and then read 4 bits from the read byte (the other 4 bits are ignored).  Then I check if the 4 bytes equals 15 (0b1111). The underlying source stream was advanced by 1 byte, so I read the next byte to discard it, and then check for eof for completeness.  

Here is another example that applies the combinator `many` to the bitParser.  This example parses each bit and returns the bit value for a byte, and is applied to an array of 10 bytes

```fsharp
[<Test>]
let testApplyManyBits() = 
    let bytes = Array.init 10 (fun i -> byte(0x01))

    let parserStream = new BinStream(new MemoryStream(bytes))   
    
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

Implementation Basics
---
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
    abstract member consume : int -> 'ConsumeType option * IStreamP<'StateType, 'ConsumeType>
    abstract member skip : int -> bool option * IStreamP<'StateType, 'ConsumeType>
    abstract member backtrack : unit -> unit
    abstract member hasMore : unit -> bool
    abstract member equals : IStreamP<'StateType, 'ConsumeType> -> bool
```

An unfortunate side effect of this is that I ran into a value restriction because of the generics, so every combinator needs to include a generic declaration like 

`let chars<'a> = //..whatever"` 
 
Included is a set of nunit tests that demonstrates the combinators (both string and binary).
