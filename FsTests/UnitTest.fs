namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Combinator.Combinator
open StringCombinators
open Combinator
open FooFighterMatcher.FooSample

[<TestClass>]
type UnitTest() = 
    [<TestMethod>]
    member this.preturn () = 
        let target = new StringStreamP("foofighters")

        let band = test target band
        
        match band with
            | FooFighter -> Assert.IsTrue true
            | _ -> Assert.IsFalse true

    [<TestMethod>]
    member this.many () = 
        
        let manyFooStr = test (new StringStreamP("foofoofoofoofob")) manyFoo

        Assert.IsTrue (List.length manyFooStr = 4)

    [<TestMethod>]
    member this.fooString () = 
        let target = new StringStreamP("foofighters")
        
        let fString = test target fooString

        fString = "foo" |> Assert.IsTrue 

    [<TestMethod>]
    member this.fightString () = 
        let target = new StringStreamP("foofighters")

        let fightString = test target fighterString
        
        fightString = "fighter" |> Assert.IsTrue 

    [<TestMethod>]
    member this.testTuples () = 
        let target = new StringStreamP("foofighters")

        let (foo, fighters) = test target fighterTuples

        foo = "foo" |> Assert.IsTrue
        fighters = "fighter" |> Assert.IsTrue

        
    [<TestMethod>]
    member this.options () = 
        let target = new StringStreamP("foofighters")
        
        test target opts = "foo" |> Assert.IsTrue

        test target optsC = "foo" |> Assert.IsTrue

    [<TestMethod>]
    member this.manyOptions () = 
        let target = new StringStreamP("foofighters")
        
        test target (many opts) = ["foo";"fighter"] |> Assert.IsTrue
        test target (many optsC) = ["foo";"fighter"] |> Assert.IsTrue

    [<TestMethod>]
    member this.regex () = 
        let target = new StringStreamP("foofighters")
        
        test target fRegex = "foof" |> Assert.IsTrue

    [<TestMethod>]
    member this.regexes () = 
        let target = new StringStreamP("      foofighters           foofighters")
        
        let result = test target fooFightersWithSpaces
        
        result |> List.length = 4 |> Assert.IsTrue

    [<TestMethod>]
    member this.anyOfChars () = 
        let target = new StringStreamP("      foofighters           foofighters") :> IStreamP<string>
        
        let result = test target allFooCharacters |> List.fold (+) ""
        
        result = target.state |> Assert.IsTrue

    [<TestMethod>]
    member this.newLine () = 
        let fullNewline = new StringStreamP("\r\n")  :> IStreamP<string>       
        let carriageReturn = new StringStreamP("\r") :> IStreamP<string>
        let newLine = new StringStreamP("\n")  :> IStreamP<string>    
        let nl = @"
"
        let newLine2 = new StringStreamP(nl) :> IStreamP<string>

        test fullNewline newline = fullNewline.state |> Assert.IsTrue
        test carriageReturn newline = carriageReturn.state |> Assert.IsTrue
        test newLine newline = newLine.state |> Assert.IsTrue
        test newLine2 newline = newLine2.state |> Assert.IsTrue

    [<TestMethod>]
    member this.attempt () = 
        let target = new StringStreamP("foofighters")
        
        match test target parseWithErrorAttempt with
            | FooFighter -> Assert.IsTrue true
        