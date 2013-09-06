module FooStringUnitTests 

open System
open NUnit.Framework
open FsUnit
open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase
open StringMatchers.FooSample

[<Test>]
let shortCircuitOr () = 
    let target = makeStringStream "fab"

    let x = choice[matchStr "f"; matchStr "a"; matchStr "b"]

    let band = test target x
        
    () |> should equal ()

[<Test>]
let preturnTest () = 
    let target = makeStringStream "foofighters"

    let band = test target band
        
    match band with
        | FooFighter -> Assert.IsTrue true        

[<Test>]
let manyTest () =         
    let manyFooStr = test (makeStringStream "foofoofoofoofob") manyFoo

    Assert.IsTrue (List.length manyFooStr = 4)

[<Test>]
let fooString () = 
    let target = makeStringStream "foofighters"
        
    let fString = test target fooString

    fString = "foo" |> Assert.IsTrue 

[<Test>]
let fightString () = 
    let target = makeStringStream "foofighters"

    let fightString = test target fighterString
        
    fightString = "fighter" |> Assert.IsTrue 

[<Test>]
let testTuples () = 
    let target = makeStringStream "foofighters"

    let (foo, fighters) = test target fighterTuples

    foo = "foo" |> Assert.IsTrue
    fighters = "fighter" |> Assert.IsTrue

        
[<Test>]
let options () = 
    let target = makeStringStream "foofighters"
        
    test target opts = "foo" |> Assert.IsTrue

    test target optsC = "foo" |> Assert.IsTrue

[<Test>]
let manyOptions () = 
    let target = makeStringStream "foofighters" |> toInterface
        
    test target (many opts) = ["foo";"fighter"] |> Assert.IsTrue
    test target (many optsC) = ["foo";"fighter"] |> Assert.IsTrue

[<Test>]
let regex () = 
    let target = makeStringStream "foofighters"
        
    test target fRegex = "foof" |> Assert.IsTrue

[<Test>]
let regexes () = 
    let target = makeStringStream "      foofighters           foofighters"
        
    let result = test target fooFightersWithSpaces
        
    result |> List.length = 4 |> Assert.IsTrue

[<Test>]
let anyOfChars () = 
    let target = makeStringStream "      foofighters           foofighters" |> toInterface
        
    let result = test target allFooCharacters |> List.fold (+) ""
        
    result = target.state |> Assert.IsTrue

[<Test>]
let newLine () = 
    let fullNewline = makeStringStream "\r\n"  |> toInterface
    let carriageReturn = makeStringStream "\r" |> toInterface
    let newLine = makeStringStream "\n"  |> toInterface
    let nl = @"
"
    let newLine2 = makeStringStream(nl) |> toInterface

    test fullNewline newline = fullNewline.state |> Assert.IsTrue
    test carriageReturn newline = carriageReturn.state |> Assert.IsTrue
    test newLine newline = newLine.state |> Assert.IsTrue
    test newLine2 newline = newLine2.state |> Assert.IsTrue

[<Test>]
let attemptTest () = 
    let target = makeStringStream "foofighters" 
        
    match test target parseWithErrorAttempt with
        | FooFighter -> Assert.IsTrue true

[<Test>]
let manyTillTest () =
    let target = makeStringStream "abc abc def abc" 

    let abc = matchStr "abc" .>> ws

    let def = matchStr "def"

    let line = (manyTill abc def .>> ws) .>>. abc .>> eof

    let result = test target line

    result |> should equal (["abc";"abc"],"abc")

[<Test>]
[<ExpectedException>]
let manyTillOneOrMore () =
    let target = makeStringStream "x abc def abc"

    let abc = matchStr "abc" .>> ws

    let def = matchStr "def"

    let line = (manyTill1 abc def .>> ws) .>>. abc .>> eof

    let result = test target line

    result |> should equal (["abc";"abc"],"abc")

[<Test>]
let lookaheadTest () =
    let target = makeStringStream "abc abc def abc" |> toInterface
    
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
    let target = makeStringStream "abc abc def abc" |> toInterface
    
    let foo = matchStr "foo"

    let manyFoo = many1 foo

    test target manyFoo |> should equal false

[<Test>]
let many1Test () = 
    let target = makeStringStream "abc abc def abc" |> toInterface
    
    let abc = ws >>. matchStr "abc"

    let manyAbc = many1 abc

    test target manyAbc |> should equal ["abc";"abc"]

[<Test>]
let testForwardingRefP() = 
    let target = makeStringStream "{abc}" |> toInterface

    let abc = matchStr "abc"
    
    let impl, fwd = createParserForwardedToRef()

    fwd := between (matchStr "{") abc (matchStr "}")

    let result = test target impl

    result |> should equal "abc"

[<Test>]
let testForwardingRefPRecursive() = 
    let target = makeStringStream "{a{a{a{a{a}}}}}" 

    
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
    let target = makeStringStream "abc" |> toInterface

    let abc = matchStr "abc"

    let a = matchStr "a" >>= fun a ->
            matchStr "b" >>= fun b ->
            matchStr "c" >>= fun c -> preturn (a + b + c + "foo")

    let elevator = fun i s -> makeStringStream(i) |> toInterface
        
    let r = reproc elevator (abc .>> eof) a

    let result = test target r

    result |> should equal "abcfoo"