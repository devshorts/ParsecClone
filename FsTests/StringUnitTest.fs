namespace UnitTestProject1

open System
open NUnit.Framework
open FsUnit

open Combinator
open StringCombinator
open StringMatchers.FooSample


module FooStringUnitTests = 
    [<Test>]
    let preturn () = 
        let target = new StringStreamP("foofighters")

        let band = test target band
        
        match band with
            | FooFighter -> Assert.IsTrue true
            | _ -> Assert.IsFalse true

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
    let attempt () = 
        let target = new StringStreamP("foofighters")
        
        match test target parseWithErrorAttempt with
            | FooFighter -> Assert.IsTrue true

