namespace FooFighterMatcher

open StringCombinator.Combinator

module FooSample = 
    
    type Band = 
        | FooFighter

    let foo = matchStr "foo"
    let fighter = matchStr "fighter"
    
    let band = foo >>. fighter |>>% FooFighter

    let fooString = foo .>> fighter 
    let fighterString = foo >>. fighter 
    let fighterTuples = foo .>>. fighter     

    let err =         
        fighter >>=? fun v -> 
                     foo

    let manyFoo = many foo
    