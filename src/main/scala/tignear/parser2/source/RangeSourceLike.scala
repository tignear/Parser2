package tignear.parser2.source

trait RangeSourceLike[@specialized +Value,+RValue,+That<:RangeSourceLike[Value,RValue,That]] extends Source[Value] with SourceLike[Value,That]{
  def peekRange(len:Int):RValue
  def getRange(len:Int):(RValue,That)
  def skip(len:Int):That

  override def peek:Value

  override def peekOption :Option[Value]

  override def next : That

  override def get : (Value,That)

  override def getOption :Option[(Value,That)]
}
