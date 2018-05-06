import tignear.parser2.Parser.IO.NotIO
import tignear.parser2.{Parser, Parsers}
import tignear.parser2.source.{RangeSourceLike, SeqSource, SourceLike}
import tignear.parser2.Parsers._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

object Free {
  def main(args: Array[String]): Unit = {
    val seqs=new SeqSource("next' step".toIndexedSeq)
    implicit val cbf:CanBuildFrom[Nothing,Char,String]=new CanBuildFrom[Nothing,Char,String]{
      def sb= StringBuilder.newBuilder
      override def apply(from: Nothing) =sb
      override def apply() = sb
    }
    implicit val cbf2:CanBuildFrom[Nothing,String,String]=new CanBuildFrom[Nothing,String,String]{
      def sb:mutable.Builder[String,String]=new mutable.Builder[String,String] {
        val sbi=StringBuilder.newBuilder
        override def +=(elem: String) = {sbi++=elem;this}

        override def clear(): Unit = sbi.clear()

        override def result() = sbi.result()
      }
      override def apply(from: Nothing) =sb
      override def apply() = sb
    }
    type I[+KS<:I[KS]]=RangeSourceLike[Char,Seq[Char],KS]
    val p:Parser[I,String]=repeat[I,Char,String](get[I,Char],4)
    println(p.apply(seqs))
    val p1=wrapOption[I,String](join[I,NotIO,String,String](map[I,Seq[Char],String](get[I,Seq[Char]](4))(e=>e.mkString),supply[I,NotIO,String]("!?")))
    val r=joinOption[I,NotIO,String,String](p1,skip[I,String](2),wrapOption[I,String](repeat[I,String,String](supply[I,NotIO,String](" "),2)),p1)
    println(r.apply(seqs))
  }
}
