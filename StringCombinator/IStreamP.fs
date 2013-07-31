namespace Combinator

type IStreamP<'Y> =
    abstract member consume : 'Y -> IStreamP<'Y>
    abstract member state : 'Y

