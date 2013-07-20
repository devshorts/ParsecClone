namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open StringCombinator.Combinator
open StringCombinator.StringP
open FooFighterMatcher.FooSample

[<TestClass>]
type UnitTest() = 
    [<TestMethod>]
    member this.preturn () = 
        let target = "foofighters"

        let band = test target band
        
        match band with
            | FooFighter -> Assert.IsTrue true
            | _ -> Assert.IsFalse true

    [<TestMethod>]
    member this.many () = 
        
        let manyFooStr = test "foofoofoofoofob" manyFoo

        Assert.IsTrue (List.length manyFooStr = 4)

    [<TestMethod>]
    member this.fooString () = 
        let target = "foofighters"
        
        let fString = test target fooString

        fString = "foo" |> Assert.IsTrue 

    [<TestMethod>]
    member this.fightString () = 
        let target = "foofighters"

        let fightString = test target fighterString
        
        fightString = "fighter" |> Assert.IsTrue 

    [<TestMethod>]
    member this.testTuples () = 
        let target = "foofighters"

        let (foo, fighters) = test target fighterTuples

        foo = "foo" |> Assert.IsTrue
        fighters = "fighter" |> Assert.IsTrue

        
    [<TestMethod>]
    member this.options () = 
        let target = "foofighters"
        
        test target opts = "foo" |> Assert.IsTrue

    [<TestMethod>]
    member this.manyOptions () = 
        let target = "foofighters"
        
        test target (many opts) = ["foo";"fighter"] |> Assert.IsTrue

    [<TestMethod>]
    member this.regex () = 
        let target = "foofighters"
        
        test target fRegex = "foof" |> Assert.IsTrue

    [<TestMethod>]
    member this.regexes () = 
        let target = "      foofighters           foofighters"
        
        let result = test target fooFightersWithSpaces
        
        result |> List.length = 4 |> Assert.IsTrue

    [<TestMethod>]
    member this.anyOfChars () = 
        let target = "      foofighters           foofighters"
        
        let result = test target allFooCharacters |> List.fold (+) ""
        
        result = target |> Assert.IsTrue

    [<TestMethod>]
    member this.newLine () = 
        let fullNewline = "\r\n"        
        let carriageReturn = "\r"        
        let newLine = "\n"     
        let newLine2 = @"
"

        test fullNewline newline = fullNewline |> Assert.IsTrue
        test carriageReturn newline = carriageReturn |> Assert.IsTrue
        test newLine newline = newLine |> Assert.IsTrue
        test newLine2 newline = newLine2 |> Assert.IsTrue

    