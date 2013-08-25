module FooStringUnitTests 

open System
open NUnit.Framework
open FsUnit
open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase
open StringMatchers.FooSample


[<Test>]
let preturnTest () = 
    let target = new StringStreamP("foofighters")

    let band = test target band
        
    match band with
        | FooFighter -> Assert.IsTrue true        

[<Test>]
let manyTest () = 
        
    let manyFooStr = test (new StringStreamP("foofoofoofoofob")) manyFoo

    Assert.IsTrue (List.length manyFooStr = 4)

[<Test>]
let fooString () = 
    let target = new StringStreamP("foofighters")
        
    let fString = test target fooString

    fString = "foo" |> Assert.IsTrue 

[<Test>]
let fightString () = 
    let target = new StringStreamP("foofighters")

    let fightString = test target fighterString
        
    fightString = "fighter" |> Assert.IsTrue 

[<Test>]
let testTuples () = 
    let target = new StringStreamP("foofighters")

    let (foo, fighters) = test target fighterTuples

    foo = "foo" |> Assert.IsTrue
    fighters = "fighter" |> Assert.IsTrue

        
[<Test>]
let options () = 
    let target = new StringStreamP("foofighters")
        
    test target opts = "foo" |> Assert.IsTrue

    test target optsC = "foo" |> Assert.IsTrue

[<Test>]
let manyOptions () = 
    let target = new StringStreamP("foofighters") :> IStreamP<string, string>
        
    test target (many opts) = ["foo";"fighter"] |> Assert.IsTrue
    test target (many optsC) = ["foo";"fighter"] |> Assert.IsTrue

[<Test>]
let regex () = 
    let target = new StringStreamP("foofighters")
        
    test target fRegex = "foof" |> Assert.IsTrue

[<Test>]
let regexes () = 
    let target = new StringStreamP("      foofighters           foofighters")
        
    let result = test target fooFightersWithSpaces
        
    result |> List.length = 4 |> Assert.IsTrue

[<Test>]
let anyOfChars () = 
    let target = new StringStreamP("      foofighters           foofighters") :> IStreamP<string, string>
        
    let result = test target allFooCharacters |> List.fold (+) ""
        
    result = target.state |> Assert.IsTrue

[<Test>]
let newLine () = 
    let fullNewline = new StringStreamP("\r\n")  :> IStreamP<string, string>
    let carriageReturn = new StringStreamP("\r") :> IStreamP<string, string>
    let newLine = new StringStreamP("\n")  :> IStreamP<string, string>
    let nl = @"
"
    let newLine2 = new StringStreamP(nl) :> IStreamP<string, string>

    test fullNewline newline = fullNewline.state |> Assert.IsTrue
    test carriageReturn newline = carriageReturn.state |> Assert.IsTrue
    test newLine newline = newLine.state |> Assert.IsTrue
    test newLine2 newline = newLine2.state |> Assert.IsTrue

[<Test>]
let attemptTest () = 
    let target = new StringStreamP("foofighters")
        
    match test target parseWithErrorAttempt with
        | FooFighter -> Assert.IsTrue true

[<Test>]
let manyTillTest () =
    let target = new StringStreamP("abc abc def abc")

    let abc = matchStr "abc" .>> ws

    let def = matchStr "def"

    let line = (manyTill abc def .>> ws) .>>. abc .>> eof

    let result = test target line

    result |> should equal (["abc";"abc"],"abc")

[<Test>]
[<ExpectedException>]
let manyTillOneOrMore () =
    let target = new StringStreamP("x abc def abc")

    let abc = matchStr "abc" .>> ws

    let def = matchStr "def"

    let line = (manyTill1 abc def .>> ws) .>>. abc .>> eof

    let result = test target line

    result |> should equal (["abc";"abc"],"abc")

[<Test>]
let lookaheadTest () =
    let target = new StringStreamP("abc abc def abc") :> IStreamP<string, string>
    
    let abc = lookahead (matchStr "abc" .>> ws) >>= fun r -> 
        if r = "abc" then preturn "found"
        else preturn "not found"

    match abc target with 
        | Some(m), state -> 
            m |> should equal "found"
            state.state |> should equal target.state
        | None, state -> 
            false |> should equal true

[<Test>]
[<ExpectedException>]
let many1TestFail () = 
    let target = new StringStreamP("abc abc def abc") :> IStreamP<string, string>
    
    let foo = matchStr "foo"

    let manyFoo = many1 foo

    test target manyFoo |> should equal false

[<Test>]
let many1Test () = 
    let target = new StringStreamP("abc abc def abc") :> IStreamP<string, string>
    
    let abc = ws >>. matchStr "abc"

    let manyAbc = many1 abc

    test target manyAbc |> should equal ["abc";"abc"]

[<Test>]
let testForwardingRefP() = 
    let target = new StringStreamP("{abc}") :> IStreamP<string, string> 

    let abc = matchStr "abc"
    
    let impl, fwd = createParserForwardedToRef()

    fwd := between (matchStr "{") abc (matchStr "}")

    let result = test target impl

    result |> should equal "abc"

[<Test>]
let testForwardingRefPRecursive() = 
    let target = new StringStreamP("{a{a{a{a{a}}}}}")

    
    let impl, fwd = createParserForwardedToRef()

    let a  = matchStr "a"    
    let lB = matchStr "{"
    let rB = matchStr "}"

    let brak = between lB (a .>> opt impl) rB

    fwd := brak

    let result = test target (impl .>> eof)

    result |> should equal "a"

[<Test>]
let reprocessTest() = 
    let target = new StringStreamP("abc") :> IStreamP<string, string> 

    let abc = matchStr "abc"

    let a = matchStr "a" >>= fun a ->
            matchStr "b" >>= fun b ->
            matchStr "c" >>= fun c -> preturn (a + b + c + "foo")

    let elevator = (fun i -> new StringStreamP(i) :> IStreamP<string, string>)
        
    let r = reproc elevator (abc .>> eof) a

    let result = test target r

    result |> should equal "abcfoo"