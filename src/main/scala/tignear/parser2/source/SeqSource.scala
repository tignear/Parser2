package tignear.parser2.source

class SeqSource[@specialized +Value](seq:Seq[Value]) extends RangeSource [Value,Seq[Value]] with RangeSourceLike [Value,Seq[Value],SeqSource[Value]]{

  override def peekRange(len: Int) = seq.take(len)

  override def getRange(len: Int) = seq.splitAt(len) match {
    case (a, b) => (a, new SeqSource[Value](b))
  }


  override def skip(len: Int) = new SeqSource[Value](seq.drop(len))

  override def peek = seq.head


  override def peekOption = seq.headOption


  override def next = new SeqSource(seq.tail)


  override def get = {
    (seq.head,new SeqSource[Value](seq.tail))
  }

  override def getOption = seq.headOption.map(e=>(e,new SeqSource[Value](seq.tail)))
}
