package net.timerboard

import scala.compiletime.erasedValue

trait SimpleEnumDerivationHelpers {
  inline def getNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => valueOf[t].asInstanceOf[String] :: getNames[ts]

  inline def getValues[P, T <: Tuple]: List[P] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => valueOf[t].asInstanceOf[P] :: getValues[P, ts]

}
