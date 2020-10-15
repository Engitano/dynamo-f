package com.engitano.dynamof.formats

import cats.implicits._
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter
import scala.util.Try
import java.time.LocalDateTime
import java.time.ZonedDateTime
import shapeless.HList
import shapeless._
import shapeless.labelled._
import eu.timepit.refined.collection._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import com.engitano.dynamof.formats.implicits._

import java.time.LocalDate
import java.time.OffsetDateTime

sealed trait DynamoUnmarshallException extends Throwable
case object EmptyStringException          extends DynamoUnmarshallException
case object AttributeValueFormatException extends DynamoUnmarshallException
case class BaseCaseNotPossibleException(fieldname: String, dv: DynamoValue)  extends DynamoUnmarshallException {
  override def getMessage(): String = s"Cannot generate type class for field name $fieldname and dynamo value $dv"
}

case class UnknownMarshallingException(cause: String)  extends DynamoUnmarshallException {
  override def getMessage(): String = cause
}


private[formats] object DateFormats {
  val zonedFormatter      = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  val localTimedFormatter = DateTimeFormatter.ISO_DATE_TIME
  val localDateFormatter  = DateTimeFormatter.ISO_DATE
}

object UnsafeString {
  import DynamoValue._
  implicit def toDynamoValueForString: ToDynamoValue[String] = new ToDynamoValue[String] {
    def to(a: String): DynamoValue = S(a)
  }
}

trait LowPriorityToAttributeValue {

  import DynamoValue._

  implicit def toDynamoValueForBool: ToDynamoValue[Boolean] = new ToDynamoValue[Boolean] {
    def to(b: Boolean) = Bool(b)
  }
  implicit def toDynamoValueForInt: ToDynamoValue[Int] = new ToDynamoValue[Int] {
    def to(i: Int) = N(i.toString())
  }
  implicit def toDynamoValueForLong: ToDynamoValue[Long] = new ToDynamoValue[Long] {
    def to(i: Long) = N(i.toString())
  }
  implicit def toDynamoValueForFloat: ToDynamoValue[Float] = new ToDynamoValue[Float] {
    def to(i: Float) = N(i.toString())
  }
  implicit def toDynamoValueForDouble: ToDynamoValue[Double] = new ToDynamoValue[Double] {
    def to(i: Double) = N(i.toString())
  }
  implicit def toDynamoValueForByteBuffer: ToDynamoValue[ByteBuffer] = new ToDynamoValue[ByteBuffer] {
    def to(i: ByteBuffer) = B(i)
  }

  implicit def toDynamoValueForDynamoString: ToDynamoValue[DynamoString] = new ToDynamoValue[DynamoString] {
    def to(s: DynamoString) = S(s.toString)
  }

  implicit def toDynamoValueForLocalDate: ToDynamoValue[LocalDate] =
    new ToDynamoValue[LocalDate] {
      def to(s: LocalDate) = S(DateFormats.localDateFormatter.format(s))
    }

  implicit def toDynamoValueForLocalDateTime: ToDynamoValue[LocalDateTime] =
    new ToDynamoValue[LocalDateTime] {
      def to(s: LocalDateTime) = S(DateFormats.localTimedFormatter.format(s))
    }

  implicit def toDynamoValueForZonedDateTime: ToDynamoValue[ZonedDateTime] =
    new ToDynamoValue[ZonedDateTime] {
      def to(s: ZonedDateTime) = S(DateFormats.zonedFormatter.format(s))
    }

  implicit def toDynamoValueForOffsetDateTime: ToDynamoValue[OffsetDateTime] =
    new ToDynamoValue[OffsetDateTime] {
      def to(s: OffsetDateTime) = S(DateFormats.zonedFormatter.format(s))
    }

  implicit def toDynamoValueForOption[T](implicit tav: ToDynamoValue[T]): ToDynamoValue[Option[T]] =
    new ToDynamoValue[Option[T]] {
      def to(b: Option[T]) = b match {
        case None    => Null
        case Some(t) => tav.to(t)
      }
    }

  implicit def toDynamoValueForSeq[C[_], T](implicit tav: ToDynamoValue[T], ev: C[T] <:< Seq[T]): ToDynamoValue[C[T]] =
    new ToDynamoValue[C[T]] {
      def to(b: C[T]) = L(b.map(t => tav.to(t)).toList)
    }

  implicit def toDynamoValueForSet[T](implicit tav: ToDynamoValue[T]): ToDynamoValue[Set[T]] =
    new ToDynamoValue[Set[T]] {
      def to(b: Set[T]) = L(b.map(t => tav.to(t)).toList)
    }

  implicit def toDynamoValueForToDynamoMappable[T](implicit tdm: Lazy[ToDynamoMap[T]]): ToDynamoValue[T] =
    new ToDynamoValue[T] {
      def to(b: T) = tdm.value.to(b)
    }
}

trait LowPriorityFromAttributeValue {

  import DynamoValue._

  private def attempt[T](f: PartialFunction[DynamoValue, T]): FromDynamoValue[T] =
    new FromDynamoValue[T] {
      def from(dv: DynamoValue) = (Try(f.lift(dv)).toEither match {
        case Right(Some(d)) => Right(d)
        case _              => Left(AttributeValueFormatException)
      })
    }

  private def attemptEither[T](f: PartialFunction[DynamoValue, Either[Throwable, T]]): FromDynamoValue[T] =
    new FromDynamoValue[T] {
      def from(dv: DynamoValue) = Try(f.lift(dv)).toEither.flatMap {
        case Some(d) => d
        case _              => Left(AttributeValueFormatException)
      }.leftMap(_ => AttributeValueFormatException)
    }

  implicit val fromAttributeValueForBool: FromDynamoValue[Boolean] =
    attempt[Boolean] {
      case Bool(b) => b
    }

  implicit val fromAttributeValueForInt: FromDynamoValue[Int] = attempt {
    case N(n) => n.toInt
  }
  implicit val fromAttributeValueForLong: FromDynamoValue[Long] = attempt {
    case N(n) => n.toLong
  }
  implicit val fromAttributeValueForFloat: FromDynamoValue[Float] = attempt {
    case N(n) => n.toFloat
  }
  implicit val fromAttributeValueForDouble: FromDynamoValue[Double] = attempt {
    case N(n) => n.toDouble
  }

  implicit val fromAttributeValueForString: FromDynamoValue[String] = attempt {
    case S(s) => s
  }

  implicit val fromAttributeValueForNonEmptyString: FromDynamoValue[DynamoString] =
    attemptEither {
      case S(s) if (s.length() > 0) => refineV[NonEmpty](s).leftMap(_ => EmptyStringException)
    }

  implicit val fromAttributeValueForLocalDate: FromDynamoValue[LocalDate] = attemptEither {
    case S(s) => Either.catchNonFatal(LocalDate.parse(s, DateFormats.localDateFormatter))
  }

  implicit val fromAttributeValueForLocalDateTime: FromDynamoValue[LocalDateTime] =
    attempt {
      case S(s) => LocalDateTime.parse(s, DateFormats.localTimedFormatter)
    }

  implicit val fromAttributeValueForZonedDateTime: FromDynamoValue[ZonedDateTime] =
    attempt {
      case S(s) => ZonedDateTime.parse(s, DateFormats.zonedFormatter)
    }

  implicit val fromAttributeValueForOffsetDateTime: FromDynamoValue[OffsetDateTime] =
    attempt {
      case S(s) => OffsetDateTime.parse(s, DateFormats.zonedFormatter)
    }

  implicit def fromAttributeValueForSeq[A](implicit
      fda: FromDynamoValue[A]
  ): FromDynamoValue[Seq[A]] =
    attemptEither {
      case L(s)  => s.traverse(fda.from).map(_.toSeq)
      case Empty => Right(Seq())
    }

  implicit def fromAttributeValueForList[A](implicit
      fda: FromDynamoValue[A]
  ): FromDynamoValue[List[A]] = fromAttributeValueForSeq[A](fda).map(_.toList)


  implicit def fromAttributeValueForSet[A](implicit
      fda: FromDynamoValue[A]
  ): FromDynamoValue[Set[A]] = fromAttributeValueForSeq[A](fda).map(_.toSet)

  implicit def fromAttributeValueForMap[A](implicit
      fda: FromDynamoValue[A]
  ): FromDynamoValue[Map[String, A]] =
    attemptEither {
      case M(s)  => s.toList.traverse(p => fda.from(p._2).map(r => p._1 -> r)).map(_.toMap)
      case Empty => Right(Map[String, A]())
    }
}

trait AutoToAttributeValue {

  import DynamoValue._

  implicit def toDynamoMapForMap[V](implicit tmv: ToDynamoValue[V]): ToDynamoMap[Map[String, V]] = new ToDynamoMap[Map[String, V]] {
    def to(av: Map[String, V]) = M(av.view.mapValues(tmv.to).toMap)
  }

  implicit def toDynamoMapForTuple[V](implicit tmv: ToDynamoValue[V]): ToDynamoMap[(String, V)] = new ToDynamoMap[(String, V)] {
    def to(av: (String, V)) = toDynamoMapForMap[V].to(Map(av))
  }

  implicit def toDynamoMapForHNil: ToDynamoMap[HNil] = new ToDynamoMap[HNil] {
    def to(av: HNil) = M(Map())
  }

  implicit def toDynamoMapForHCons[Key <: Symbol, H, T <: HList](
      implicit
      key: Witness.Aux[Key],
      th: Lazy[ToDynamoValue[H]],
      tt: Lazy[ToDynamoMap[T]]
  ): ToDynamoMap[FieldType[Key, H] :: T] = new ToDynamoMap[FieldType[Key, H] :: T] {
    def to(a: labelled.FieldType[Key, H] :: T): DynamoValue.M = {
      val pair = key.value.name -> th.value.to(a.head)
      DynamoValue.M(tt.value.to(a.tail).m + pair)
    }
  }

  implicit def fromLabelledGeneric[T, R <: HList](
      implicit gen: LabelledGeneric.Aux[T, R],
      TDM: ToDynamoMap[R]
  ): ToDynamoMap[T] =
    new ToDynamoMap[T] {
      def to(a: T): DynamoValue.M = TDM.to(LabelledGeneric[T].to(a))
    }
}

trait CaseClassDerivations {

  import DynamoValue._

  implicit def fromAttributeValueForHNil: FromDynamoValue[HNil] = new FromDynamoValue[HNil] {
    def from(av: DynamoValue): Either[DynamoUnmarshallException, HNil] = Right(HNil)
  }

  implicit def fromAttributeValueForHConsOption[ Key <: Symbol, H, T <: HList](implicit
      key: Witness.Aux[Key],
      th: Lazy[FromDynamoValue[H]],
      tt: Lazy[FromDynamoValue[T]]
  ): FromDynamoValue[FieldType[Key, Option[H]] :: T] = new FromDynamoValue[FieldType[Key, Option[H]] :: T] {
    def from(av: DynamoValue): Either[DynamoUnmarshallException, labelled.FieldType[Key, Option[H]] :: T] = av match {
      case M(m) =>
        val v: Either[DynamoUnmarshallException, FieldType[Key, Option[H]]] = (m.get(key.value.name) match {
          case Some(Null) => Right(none[H])
          case Some(v)    => th.value.from(v).map(_.some)
          case None       => Right(none[H])
        }).map(v => field[Key](v))

        val t = tt.value.from(av)
        (v, t).tupled.map {
          case (v, t) => v :: t
        }
      case _ => Left(BaseCaseNotPossibleException(key.value.name, av))
    }
  }

  implicit def fromAttributeValueForHCons[Key <: Symbol, H, T <: HList](implicit 
      key: Witness.Aux[Key],
      th: Lazy[FromDynamoValue[H]],
      tt: Lazy[FromDynamoValue[T]]
  ): FromDynamoValue[FieldType[Key, H] :: T] = new FromDynamoValue[FieldType[Key, H] :: T] {
    def from(av: DynamoValue): Either[DynamoUnmarshallException, labelled.FieldType[Key, H] :: T] = av match {
      case M(m) =>
        val v = m
          .get(key.value.name)
          .toRight(AttributeValueFormatException)
          .flatMap(th.value.from)
          .map(v => field[Key](v))
        val t = tt.value.from(av)
        (v, t).tupled.map {
          case (v, t) => v :: t
        }
      case _ => Left(BaseCaseNotPossibleException(key.value.name, av))
    }
  }

  implicit def fromAttributeValueForGeneric[T, R <: HList](
      implicit gen: LabelledGeneric.Aux[T, R],
      fav: Lazy[FromDynamoValue[R]]
  ): FromDynamoValue[T] = new FromDynamoValue[T] {
    def from(av: DynamoValue): Either[DynamoUnmarshallException, T] = fav.value.from(av).map(gen.from)
  }
}

trait SumTypeDerivations {

  implicit def toDynamoValueCNil: ToDynamoValue[CNil] = new ToDynamoValue[CNil] {
    def to(a: CNil): DynamoValue = DynamoValue.Empty
  }

  implicit def toDynamoValueCCons[K <: Symbol, V, R <: Coproduct](
      implicit
      fieldWitness: Witness.Aux[K],
      headGen: Lazy[ToDynamoValue[V]],
      tdvR: Lazy[ToDynamoValue[R]]
  ): ToDynamoValue[FieldType[K, V] :+: R] = new ToDynamoValue[FieldType[K, V] :+: R] {
    def to(a: labelled.FieldType[K, V] :+: R): DynamoValue = a match {
      case Inl(head) =>
        DynamoValue.M(Map(fieldWitness.value.name -> headGen.value.to(head)))
      case Inr(tail) => tdvR.value.to(tail)
    }
  }

  implicit def toDynamoValueForFamilies[A, Repr <: Coproduct](
      implicit
      gen: LabelledGeneric.Aux[A, Repr],
      genericFormat: ToDynamoValue[Repr]
  ): ToDynamoValue[A] =
    new ToDynamoValue[A] {
      final def to(av: A) = genericFormat.to(gen.to(av))
    }

  implicit def fromDynamoValueCNil: FromDynamoValue[CNil] =
    new FromDynamoValue[CNil] {
      def from(a: DynamoValue): Either[DynamoUnmarshallException, CNil] = Left(UnknownMarshallingException(s"Canot serializer ${a} to CNil"))
    }

  implicit def fromDynamoValueCCons[K <: Symbol, V, R <: Coproduct](
      implicit
      fieldWitness: Witness.Aux[K],
      headGen: Lazy[FromDynamoValue[V]],
      tdvR: Lazy[FromDynamoValue[R]]
  ): FromDynamoValue[FieldType[K, V] :+: R] = new FromDynamoValue[FieldType[K, V] :+: R] {
    def from(dv: DynamoValue): Either[DynamoUnmarshallException, labelled.FieldType[K, V] :+: R] = dv match {
      case DynamoValue.M(m) if (m.head._1 == fieldWitness.value.name) =>
        headGen.value.from(m.head._2).map(h => Inl(field[K](h)))
      case DynamoValue.M(_) => tdvR.value.from(dv).map(v => Inr(v))
      case _                => Left(new UnknownMarshallingException(s"Cannot deserialize object ${dv}"))
    }
  }

  implicit def fromDynamoValueForFamilies[A, Repr <: Coproduct](
      implicit
      ev: A <:!< Option[_], // Options are handled with fromAttributeValueForHConsOption
      ev1: A <:!< List[_],  // Options are handled with fromAttributeValueForHConsOption
      gen: LabelledGeneric.Aux[A, Repr],
      genericFormat: Lazy[FromDynamoValue[Repr]]
  ): FromDynamoValue[A] =
    new FromDynamoValue[A] {
      def from(dv: DynamoValue) = genericFormat.value.from(dv).map(gen.from)
    }
}

trait AutoFormats
    extends LowPriorityToAttributeValue
    with LowPriorityFromAttributeValue
    with AutoToAttributeValue 
    with CaseClassDerivations
    with SumTypeDerivations
