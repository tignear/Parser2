package tignear.parser2
import scala.language.higherKinds
import Parser._


object Parser{
  sealed trait Result[@specialized +S,@specialized +E]{
    val src:S
    val consume:Int
  }
  case class Success[@specialized +S,@specialized +E](src:S,elem: E,consume:Int) extends Result[S,E]
  case class Fail[@specialized +S](src:S,consume:Int) extends Result[S,Nothing]
  object IO{
    class NotIO extends IO
    class UseIO extends IO
    object ToBool {
      def apply[A <: IO](implicit toBool: ToBool[A]): Boolean = toBool()
    }
    trait ToBool[A <: IO] {
      def apply(): Boolean
    }
    implicit val toBoolUseIO = new ToBool[UseIO] {
      def apply() = true
    }
    implicit val toBoolTNotIO = new ToBool[NotIO] {
      def apply() = false
    }
  }
  sealed trait IO
}
trait Parser[-MS[@specialized +KS<:MS[KS]],@specialized +Res]{
  def apply[S <: MS[S]](src: S):Result[S,Res]
  type DoIO<:IO
}
trait IOParser[-MS[@specialized +KS<:MS[KS]],J<:IO,@specialized +Res] extends Parser[MS,Res]{
  override type DoIO = J
}
