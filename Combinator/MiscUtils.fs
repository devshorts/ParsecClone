namespace ParsecClone.CombinatorBase

[<AutoOpen>]
module MiscUtils = 
    let time s f =         
        let n = System.DateTime.Now
        let x = f()
        printfn "%s Took %s" s ((System.DateTime.Now - n).ToString())
        x
