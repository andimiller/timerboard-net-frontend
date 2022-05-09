package net.timerboard

import io.circe.Decoder.Result
import io.circe.*

import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.Mirror

object CirceEnumHelpers {
  trait EnumCodec[T] extends Codec[T]

  inline private def enumCodec[T](
      inline ms: Mirror.SumOf[T],
      inline names: List[String],
      inline values: List[T],
      inline name: String
  ): EnumCodec[T] = new EnumCodec[T] {
    override def apply(t: T): Json            = Json.fromString(names(ms.ordinal(t)))
    override def apply(c: HCursor): Result[T] = Decoder.decodeString
      .emap { s =>
        names.indexOf(s) match {
          case -1 => Left(s"'$s' was not a type of $name")
          case i  => Right(values(i))
        }
      }
      .apply(c)
  }

  object EnumCodec extends SimpleEnumDerivationHelpers:
    inline given derived[T](using m: Mirror.Of[T]): EnumCodec[T] =
      inline m match
        case s: Mirror.SumOf[T] =>
          enumCodec(
            s,
            getNames[s.MirroredElemLabels],
            getValues[T, s.MirroredElemTypes],
            valueOf[s.MirroredLabel]
          )

}
