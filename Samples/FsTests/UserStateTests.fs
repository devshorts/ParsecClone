module UserStateTests

open NUnit.Framework
open FsUnit
open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator

type userState = { Name: string }

[<Test>]
let testUserState() = 

    let state = new StringStreamP<userState>("foobar", { Name = "start!" })

    let st = matchStr "foo" >>= fun x -> 
             setUserState  { Name = x }

    let result = st >>. getUserState

    let r = test state result

    r |> should equal { Name = "foo" }
