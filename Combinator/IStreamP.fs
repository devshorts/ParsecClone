namespace ParsecClone.CombinatorBase

type IStreamP<'StateType, 'ConsumeType, 'UserState> =        
    abstract member state : 'StateType
    abstract member consume : int -> 'ConsumeType option * IStreamP<'StateType, 'ConsumeType, 'UserState>
    abstract member skip : int -> bool option * IStreamP<'StateType, 'ConsumeType, 'UserState>
    abstract member backtrack : unit -> unit
    abstract member hasMore : unit -> bool
    abstract member equals : IStreamP<'StateType, 'ConsumeType, 'UserState> -> bool
    abstract member canConsume : int -> int option
    abstract member getUserState :  unit -> 'UserState
    abstract member setUserState : 'UserState -> unit  
    abstract member position : unit -> int64