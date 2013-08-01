namespace Combinator

type IStreamP<'Y> =        
    abstract member state : 'Y
    //abstract member consume: IStreamP<'Y> -> 'a -> 'b option * IStreamP<'Y>

