namespace ParsecClone.CombinatorBase

module MiscUtils = 
    let time s f =         
        let n = System.DateTime.Now
        let value = f()
        printfn "%s Took %s" s ((System.DateTime.Now - n).ToString())
        value
