module CompExpressionTests

open FsUnit
open NUnit.Framework
open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

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