namespace Combinator

type IStreamP<'StateType, 'ConsumeType> =        
    abstract member state : 'StateType
    abstract member consume : int -> 'ConsumeType option * IStreamP<'StateType, 'ConsumeType>
    abstract member skip : int -> bool option * IStreamP<'StateType, 'ConsumeType>
    abstract member backtrack : unit -> unit
    abstract member hasMore : unit -> bool
    abstract member equals : IStreamP<'StateType, 'ConsumeType> -> bool
    abstract member canConsume : int -> int option