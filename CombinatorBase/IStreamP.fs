namespace Combinator

type IStreamP<'Y, 'A> =        
    abstract member state : 'Y
    abstract member consume : IStreamP<'Y, 'A> -> int -> 'A option * IStreamP<'Y, 'A>