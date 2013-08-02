module CsvtringUnitTests 

open System
open NUnit.Framework
open FsUnit

open Combinator
open StringCombinator
open StringMatchers.CsvSample


[<Test>]
let testElement() = 
    let csv = new StringStreamP("some text")

    let result = test csv csvElement

    result |> should equal "some text"
    
[<Test>]
let testElements() = 
    let csv = new StringStreamP("some text,")

    let result = test csv element

    result |> should equal "some text"

[<Test>]
let testElement2() = 
    let csv = new StringStreamP("some text")

    let result = test csv element

    result |> should equal "some text"

[<Test>]
let testTwoElement() = 
    let csv = new StringStreamP("some text, text two")

    let result = test csv elements

    result |> should equal ["some text";" text two"]

[<Test>]
let testTwoLines() = 
    let t = @"a, b
c, d"

    let csv = new StringStreamP(t)

    let result = test csv lines

    result |> should equal [["a";" b"];["c";" d"]]
