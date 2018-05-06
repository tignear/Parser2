package tignear.typ
import scala.language.higherKinds

object Bool {
  type ![That <: TBool] = That#NOT
  sealed trait TBool {
    type &[That <: TBool] <: TBool
    type |[That <: TBool] <: TBool
    type ^[That <: TBool] <: TBool
    type ==[That <: TBool] <: TBool
    type IF[Then <: T, Else <: T, T] <: T
    private[Bool] type NOT <: TBool
  }

  final class TTrue extends TBool {
    override type &[That <: TBool] = That
    override type |[_] = TTrue
    override type ^[That <: TBool] = ![That]
    override type ==[That <: TBool] = That
    override type IF[Then <: T, Else <: T, T] = Then
    override type NOT = TFalse
  }

  final class TFalse extends TBool {
    override type &[_] = TFalse
    override type |[That <: TBool] = That
    override type ^[That <: TBool] = That
    override type ==[That <: TBool] = ![That]
    override type IF[Then <: T, Else <: T, T] = Else
    override type NOT = TTrue
  }

}
