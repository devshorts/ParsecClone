namespace Combinator

type IStreamP<'StateType, 'ConsumeType> =        
    abstract member state : 'StateType
    abstract member consume : IStreamP<'StateType, 'ConsumeType> -> int -> 'ConsumeType option * IStreamP<'StateType, 'ConsumeType>
    abstract member backtrack : unit -> unit