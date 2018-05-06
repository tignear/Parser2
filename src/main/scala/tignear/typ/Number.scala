package tignear.typ

import tignear.typ.Bool.{TBool, TFalse, ! => !!}

object Number {
  type ~[That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]]= That#BITNOT
  type <<[That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]]=That#LSHIFT
  type BIT8ZERO=BIT8[TBool,TFalse,TFalse,TFalse,TFalse,TFalse,TFalse,TFalse]
  type Z= ~[BIT8ZERO]

  trait BIT8[PB1<:TBool,PB2<:TBool,PB3<:TBool,PB4<:TBool,PB5<:TBool,PB6<:TBool,PB7<:TBool,PB8<:TBool]{
    type B1=PB1
    type B2=PB2
    type B3=PB3
    type B4=PB4
    type B5=PB5
    type B6=PB6
    type B7=PB7
    type B8=PB8
    type &[That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]]=
      BIT8[B1# &[That#B1],B2# &[That#B2],B3# &[That#B3],B4# &[That#B4],B5# &[That#B5],B6# &[That#B6],B7# &[That#B7],B8# &[That#B8]]
    type |[That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]] =
      BIT8[B1# |[That#B1],B2# |[That#B2],B3# |[That#B3],B4# |[That#B4],B5# |[That#B5],B6# |[That#B6],B7# |[That#B7],B8# |[That#B8]]
    type ^[That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]] =
      BIT8[B1# ^[That#B1],B2# ^[That#B2],B3# ^[That#B3],B4# ^[That#B4],B5# ^[That#B5],B6# ^[That#B6],B7# ^[That#B7],B8# ^[That#B8]]
    type == [That<:BIT8[_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool,_<:TBool]] =
      B1# == [That#B1] # &[B2# ==[ That# B2]] # &[B3# == [That#B3]]# &[B4# == [That#B4]]# &[B5# == [That#B5]]# &[B6# == [That#B6]]# &[B7# == [That#B7]]# &[B8# == [That#B8]]
    private[Number] type BITNOT = BIT8[!![B1],!![B2],!![B3],!![B4],!![B5],!![B6],!![B7],!![B8]]
    private[Number] type LSHIFT= BIT8[B2,B3,B4,B5,B6,B7,B8,TFalse]

  }
  trait U8[PB1<:TBool,PB2<:TBool,PB3<:TBool,PB4<:TBool,PB5<:TBool,PB6<:TBool,PB7<:TBool,PB8<:TBool] extends BIT8[PB1,PB2,PB3,PB4,PB5,PB6,PB7,PB8]{

  }
}
