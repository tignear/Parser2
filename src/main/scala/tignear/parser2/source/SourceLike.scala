package tignear.parser2.source

trait SourceLike[@specialized +Value,+That<:SourceLike[Value,That]]{
  def peek:Value
  def peekOption:Option[Value]
  def next:That
  def get:(Value,That)
  def getOption:Option[(Value,That)]
}
