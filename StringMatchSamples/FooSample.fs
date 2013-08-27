namespace StringMatchers

open ParsecClone.StringCombinator
open ParsecClone.CombinatorBase

module FooSample = 
        
    type FooParser<'Return> = Parser<'Return, string, string, unit>

    type Band = 
        | FooFighter

    let foo : FooParser<_>  = matchStr "foo" 

    let fighter : FooParser<_> = matchStr "fighter" 

    let fRegex : FooParser<_> = regexStr "fo{2}f" 

    let fooFightersWithSpaces : FooParser<_> = many (whitespaces <|> chars)
    
    let band : FooParser<_> = foo >>. fighter |>>% FooFighter

    let fooString : FooParser<_> = foo .>> fighter 
    let fighterString : FooParser<_>  = foo >>. fighter 
    let fighterTuples : FooParser<_> = foo .>>. fighter     

    let validFooChar : FooParser<_> = anyOf matchStr ["f";"o";"i";"g";"h";"t";"e";"r";"s";" "]

    let allFooCharacters : FooParser<_> = many validFooChar

    let err : FooParser<_> =         
        fighter >>=? fun v -> 
                     foo

    let errAttempt = matchStr "fo"

    let parseWithErrorAttempt : FooParser<_> = choice[attempt (errAttempt >>. errAttempt) |>>% FooFighter;band]

    let manyFoo s = many foo s
    
    let opts<'a, 'b> = fighter <|> foo

    let optsC : FooParser<_> = choice[fighter;foo]

