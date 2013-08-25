namespace StringMatchers

open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

module FooSample = 
    
    type Band = 
        | FooFighter

    // foofighter

    let foo = matchStr "foo"
    let fighter = matchStr "fighter"   

    let fRegex<'a> = regexStr "fo{2}f"

    let fooFightersWithSpaces<'a> = many (whitespaces <|> chars)
    
    let band = foo >>. fighter |>>% FooFighter

    let fooString<'a> = foo .>> fighter 
    let fighterString<'a> = foo >>. fighter 
    let fighterTuples<'a> = foo .>>. fighter     

    let validFooChar = anyOf matchStr ["f";"o";"i";"g";"h";"t";"e";"r";"s";" "]

    let allFooCharacters<'a> = many validFooChar

    let err<'a> =         
        fighter >>=? fun v -> 
                     foo

    let errAttempt = matchStr "fo"

    let parseWithErrorAttempt<'a> = choice[attempt (errAttempt >>. errAttempt) |>>% FooFighter;band]
    let manyFoo<'a> = many foo
    
    let opts<'a> = fighter <|> foo

    let optsC<'a> = choice[fighter;foo]