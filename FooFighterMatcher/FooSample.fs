namespace FooFighterMatcher

open StringCombinator.Combinator
open StringCombinator.StringP

module FooSample = 
    
    type Band = 
        | FooFighter

    let foo = matchStr "foo"
    let fighter = matchStr "fighter"   

    let fRegex = regexStr "fo{2}f"

    let fooFightersWithSpaces = many (whitespaces <|> chars)
    
    let band = foo >>. fighter |>>% FooFighter

    let fooString = foo .>> fighter 
    let fighterString = foo >>. fighter 
    let fighterTuples = foo .>>. fighter     

    let validFooChar = anyOf matchStr ["f";"o";"i";"g";"h";"t";"e";"r";"s";" "]

    let allFooCharacters = many validFooChar

    let err =         
        fighter >>=? fun v -> 
                     foo

    let errAttempt = matchStr "fo"

    let parseWithErrorAttempt = choice[attempt (errAttempt >>. errAttempt) |>>% FooFighter;band]
    let manyFoo = many foo
    
    let opts = fighter <|> foo

    let optsC = choice[fighter;foo]