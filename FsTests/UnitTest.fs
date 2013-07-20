namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open StringCombinator.Combinator
open FooFighterMatcher.FooSample

[<TestClass>]
type UnitTest() = 
    [<TestMethod>]
    member this.testCombinator () = 
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