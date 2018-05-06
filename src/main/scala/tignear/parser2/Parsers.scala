package tignear.parser2
import source._
import Parser._
import tignear.parser2.Parser.IO.NotIO

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
object Parsers{
  def get[MS[+KS<:MS[KS]]<:SourceLike[R,KS],R]=new IOParser[MS,NotIO,R] {
    override def apply[S <: SourceLike[R, S]](src: S) = src.get match {
      case (e,s)=> Success(s,e,1)
    }
  }
  def get[MS[+KS<:MS[KS]]<:RangeSourceLike[_,R,KS],R](len:Int)=new IOParser[MS,NotIO,R] {
    override def apply[S <: MS[S]](src: S) = src.getRange(len) match {
      case(e,s)=>Success(s,e,len)
    }
  }
  def repeat[MS[+KS<:MS[KS]],R,RR](p: Parser[MS,R],len:Int)(implicit cbf:CanBuildFrom[Nothing,R,RR])=new IOParser[MS,p.DoIO,RR] {
    override def apply[S <: MS[S]](src: S):Result[S,RR] = {
      val b=cbf()
      var s:S=src
      var consume=0
      for(_<-0 until len) {
        p(s) match {
          case Success(ns, e, c) =>
            s = ns
            b += e
            consume += c
          case Fail(ns, c) =>
            s = ns
            consume += c
            return Fail(s, consume)
        }
      }
      Success(s,b.result(),consume)
    }
  }
  def join[MS[+KS<:MS[KS]],MIO<:IO,R,RR](ps:IOParser[MS,MIO,R]*)(implicit cbf:CanBuildFrom[Nothing,R,RR])=new IOParser[MS,MIO,RR] {
    override def apply[S <: MS[ S]](src: S):Result[S,RR] = {
      val b=cbf()
      var s=src
      var consume=0
      for(p<-ps){
        p(s) match {
          case Success(ns,e,c)=>
            s=ns
            b+=e
            consume+=c
          case Fail(ns,c)=>
            consume+=c
            return Fail(ns,consume)
        }
      }
      Success(s,b.result(),consume)
    }
  }
  def joinOption[MS[+KS<:MS[KS]],MIO<:IO,R,RR](ps:IOParser[MS,MIO,Option[R]]*)(implicit cbf:CanBuildFrom[Nothing,R,RR]) =new IOParser[MS,MIO,RR]{
    override def apply[S <: MS[ S]](src: S):Result[S,RR] = {
      val b=cbf()
      var s=src
      var consume=0
      for(p<-ps){
        p(s) match {
          case Success(ns,e,c)=>
            s=ns
            e match {
              case Some(e2)=>b+=e2
              case None=>
            }
            consume+=c
          case Fail(ns,c)=>
            consume+=c
            return Fail(ns,consume)
        }
      }
      Success(s,b.result(),consume)
    }
  }

  def supply[MS[+KS<:MS[KS]],MIO<:IO,R](v: =>R)=new IOParser[MS,MIO,R] {
    override def apply[S <: MS[S]](src: S) = Success(src,v,0)
  }
  def skip[MS[+KS<:MS[KS]]<:SourceLike[R,KS],R]=new IOParser[MS,NotIO,Option[R]] {
    override def apply[S <: SourceLike[Any, S]](src: S) = Success(src.next,None,1)
  }
  def skip[MS[+KS<:MS[KS]]<:RangeSourceLike[_,_,KS],R](len:Int)=new IOParser[MS,NotIO,Option[R]] {
    override def apply[S <: MS[ S]](src: S) = Success(src.skip(len),None,len)

    override type DoIO = IO.NotIO
  }

  def map[MS[+KS <: MS[KS]], R, RR](p: Parser[MS, R])(mapper: R => RR) = new IOParser[MS,p.DoIO, RR] {
    override def apply[S <: MS[S]](src: S): Result[S, RR] = p(src) match {
      case Success(s, e, c) => Success(s, mapper(e), c)
      case f: Fail[S] => f
    }

    override type DoIO = p.DoIO
  }

  def wrapOption[MS[+KS<:MS[KS]],R](p:Parser[MS,R])=map[MS,R,Option[R]](p)(e=>Option(e))

}