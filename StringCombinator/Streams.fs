namespace Combinator

type IStreamP<'Y> =
    abstract member consume : 'Y -> IStreamP<'Y>
    abstract member state : 'Y

type StringStreamP (state:string) = 
    interface IStreamP<string> with
        member this.consume target = 
            let newState = state.Remove(0, target.Length)
            new StringStreamP(newState) :> IStreamP<string>

        member this.state = state