module CsvtringUnitTests 

open System
open NUnit.Framework
open FsUnit

open Combinator
open StringCombinator
open StringMatchers.CsvSample


[<Test>]
let testEmptyWhiteSpace() = 
    let csv = new StringStreamP("")

    let result = test csv ws

    result |> should equal ""

[<Test>]
let testWhiteSpace() = 
    let csv = new StringStreamP(" ")

    let result = test csv ws

    result |> should equal " "

[<Test>]
let testElement() = 
    let csv = new StringStreamP("some text")

    let result = test csv csvElement

    result |> should equal "some text"
    
[<Test>]
let testElements() = 
    let csv = new StringStreamP("some text,")

    let result = test csv csvElement

    result |> should equal ("some text")

[<Test>]
let testTwoElement() = 
    let csv = new StringStreamP("some text, text two")

    let result = test csv elements

    result |> should equal (["some text";"text two"] |> List.map Some)

[<Test>]
let testTwoLines() = 
    let t = @"a, b
c, d"

    let csv = new StringStreamP(t)

    let result = test csv lines

    result |> should equal [["a";"b"] |> List.map Some;
                            ["c";"d"] |> List.map Some]

[<Test>]
let testEscaped() = 
    let t = @"\,"

    let csv = new StringStreamP(t)

    let result = test csv escapedChar

    result |> should equal ","

[<Test>]
let testLiteral() = 
    let t = "\"foo\""

    let csv = new StringStreamP(t)

    let result = test csv literal

    result |> should equal "foo"

[<Test>]
let testLiteral2() = 
    let t = "a\,b"

    let csv = new StringStreamP(t)

    let result = test csv normalAndEscaped

    result |> should equal "a,b"

[<Test>]
let testUnEscaped1() = 
    let t = "a,b"

    let csv = new StringStreamP(t)

    let result = test csv normalAndEscaped

    result |> should equal "a"

[<Test>]
let testCsvWithQuotes1() = 
    let t = "\"cd,fg\""

    let csv = new StringStreamP(t)

    let result = test csv lines

    result |> should equal [["cd,fg"] |> List.map Some]

[<Test>]
let testEmpties() =
    let t = ",,,"

    let csv = new StringStreamP(t)

    let result = test csv lines

    result |> should equal [[None;None;None;None]]

[<Test>]
let testCsvWithQuotes2() = 
    let t = "a,\"b 1.\\\",\"cd,fg\"
a,b,\\\", \"cd,fg\",,"

    let csv = new StringStreamP(t)

    let result = test csv lines |> List.toArray

    result.[0] |> should equal (["a";"b 1.\\";"cd,fg"] |> List.map Some)
    result.[1] |> should equal ([Some("a");Some("b");Some("\"");Some("cd,fg");None;None])

[<Test>]
let testCsvWithOptionalElements() = 
    let t = ",,"

    let csv = new StringStreamP(t)

    let result = test csv lines |> List.toArray

    result.[0] |> should equal ([None; None; None])    



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


[<Test>]
let testCsvWithEscapedNewlines() = 
    let t = "a\\nb"

    let csv = new StringStreamP(t)

    let result = test csv lines |> List.toArray

    result.[0] |> should equal (["a\nb"] |> List.map Some)    

[<Test>]
let testCsvWithNewlinesInQuotes() = 
    let t = @"""a
    
b"""

    let csv = new StringStreamP(t)

    let result = test csv lines 

    result |> should equal [[@"a
    
b"] |> List.map Some]  


[<Test>]
let testReadmeExample1 () = 
    let t = "foo\,,,bar,baz\\\"
faisal rules!"

    let csv = new StringStreamP(t)

    let result = test csv lines |> List.toArray

    result.[0] |> should equal ([Some("foo,"); None; Some("bar"); Some("baz\"")])  

[<Test>]
let testDoubleQuotes () = 
    let t = "\"a\" b def \"foo\","

    let csv = new StringStreamP(t)

    let result = test csv lines |> List.toArray

    result |> should equal [[Some(@"a b def foo"); None]]

[<Test>]
[<ExpectedException>]
let testEofMissing () = 
    let t = "\"foo,\",bar,baz"

    let csv = new StringStreamP(t)

    let r = test csv (csvElement .>> eof)

    r |> should equal true

    