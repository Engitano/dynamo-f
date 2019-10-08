package com.engitano.dynamof.formats
import cats.Applicative
import cats.ApplicativeError
import cats.implicits._
import java.nio.ByteBuffer
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import scala.util.Try
import java.time.LocalDateTime
import java.time.ZonedDateTime
import shapeless.HList
import shapeless._
import shapeless.labelled._
import eu.timepit.refined.collection._
import eu.timepit.refined._
import eu.timepit.refined.auto._

import cats.MonadError
import shapeless.Lazy
import cats.CommutativeApplicative
import java.time.LocalDate

case object EmptyStringException          extends Throwable
case object AttributeValueFormatException extends Throwable
case object BaseCaseNotPossibleException  extends Throwable

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

  implicit def toDynamoValueForString: ToDynamoValue[DynamoString] = new ToDynamoValue[DynamoString] {
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

  implicit def toDynamoValueForOption[T](implicit tav: ToDynamoValue[T]): ToDynamoValue[Option[T]] =
    new ToDynamoValue[Option[T]] {
      def to(b: Option[T]) = b match {
        case None    => Null
        case Some(t) => tav.to(t)
      }
    }

  implicit def toDynamoValueForSeq[T](implicit tav: ToDynamoValue[T]): ToDynamoValue[Seq[T]] =
    new ToDynamoValue[Seq[T]] {
      def to(b: Seq[T]) = L(b.map(t => tav.to(t)).toList)
    }

  implicit def toDynamoValueForToDynamoMappable[T](implicit tdm: ToDynamoMap[T]): ToDynamoValue[T] =
    new ToDynamoValue[T] {
      def to(b: T) = tdm.to(b)
    }
}

trait LowPriorityFromAttributeValue {

  import DynamoValue._

  private def attempt[F[_], T](f: PartialFunction[DynamoValue, T])(implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, T] =
    new FromDynamoValue[F, T] {
      def from(dv: DynamoValue) = Try(f.lift(dv)).toEither match {
        case Right(Some(d)) => F.pure(d)
        case _              => F.raiseError(AttributeValueFormatException)
      }
    }

  private def attemptF[F[_], T](f: PartialFunction[DynamoValue, F[T]])(implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, T] =
    new FromDynamoValue[F, T] {
      def from(dv: DynamoValue) = Try(f.lift(dv)).toEither match {
        case Right(Some(d)) => d
        case _              => F.raiseError(AttributeValueFormatException)
      }
    }

  implicit def fromAttributeValueForBool[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, Boolean] =
    attempt[F, Boolean] {
      case Bool(b) => b
    }

  implicit def fromAttributeValueForInt[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, Int] = attempt {
    case N(n) => n.toInt
  }
  implicit def fromAttributeValueForFloat[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, Float] = attempt {
    case N(n) => n.toFloat
  }
  implicit def fromAttributeValueForDouble[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, Double] = attempt {
    case N(n) => n.toDouble
  }

  implicit def fromAttributeValueForString[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, String] = attempt {
    case S(s) => s
  }

  implicit def fromAttributeValueForNonEmptyString[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, DynamoString] =
    attemptF {
      case S(s) if (s.length() > 0) => F.fromEither(refineV[NonEmpty](s).leftMap(_ => EmptyStringException))
    }

  implicit def fromAttributeValueForLocalDate[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, LocalDate] = attemptF {
    case S(s) => F.catchNonFatal(LocalDate.parse(s, DateFormats.localDateFormatter))
  }

  implicit def fromAttributeValueForLocalDateTime[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, LocalDateTime] =
    attempt {
      case S(s) => LocalDateTime.parse(s, DateFormats.localTimedFormatter)
    }

  implicit def fromAttributeValueForZonedDateTime[F[_]](implicit F: ApplicativeError[F, Throwable]): FromDynamoValue[F, ZonedDateTime] =
    attempt {
      case S(s) => ZonedDateTime.parse(s, DateFormats.zonedFormatter)
    }

  implicit def fromAttributeValueForSeq[F[_], A](
      implicit F: ApplicativeError[F, Throwable],
      fda: FromDynamoValue[F, A]
  ): FromDynamoValue[F, List[A]] =
    attemptF {
      case L(s)  => s.traverse(fda.from)
      case Empty => F.pure(List[A]())
    }

  implicit def fromAttributeValueForMap[F[_]: CommutativeApplicative, A](
      implicit F: ApplicativeError[F, Throwable],
      fda: FromDynamoValue[F, A]
  ): FromDynamoValue[F, Map[String, A]] =
    attemptF {
      case M(s)  => s.unorderedTraverse(fda.from)
      case Empty => F.pure(Map[String, A]())
    }
}

trait AutoToAttributeValue {

  import DynamoValue._

  implicit def toDynamoMapForMap[V](implicit tmv: ToDynamoValue[V]): ToDynamoMap[Map[String, V]] = new ToDynamoMap[Map[String, V]] {
    def to(av: Map[String, V]) = M(av.mapValues(tmv.to).toMap)
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

trait AutoFromAttributeValue {

  import DynamoValue._

  implicit def fromAttributeValueForHNil[F[_]](implicit F: Applicative[F]): FromDynamoValue[F, HNil] = new FromDynamoValue[F, HNil] {
    def from(av: DynamoValue): F[HNil] = F.pure(HNil)
  }

  implicit def fromAttributeValueForHConsOption[F[_], Key <: Symbol, H, T <: HList](
    implicit F: MonadError[F, Throwable],
    key: Witness.Aux[Key],
    th: Lazy[FromDynamoValue[F, H]],
    tt: Lazy[FromDynamoValue[F, T]]
): FromDynamoValue[F, FieldType[Key, Option[H]] :: T] = new FromDynamoValue[F, FieldType[Key, Option[H]] :: T] {
  def from(av: DynamoValue): F[labelled.FieldType[Key, Option[H]] :: T] = av match {
    case M(m) =>
      val v: F[FieldType[Key, Option[H]]] = (m
        .get(key.value.name) match {
          case Some(Null) => F.pure(none[H])
          case Some(v) => th.value.from(v).map(_.some)
          case None => F.pure(none[H])
        })
        .map(v => field[Key](v))
   
      val t = tt.value.from(av)
      (v, t).tupled.map {
        case (v, t) => v :: t
      }
    case _ => F.raiseError(BaseCaseNotPossibleException)
  }
}

  implicit def fromAttributeValueForHCons[F[_], Key <: Symbol, H, T <: HList](
      implicit F: MonadError[F, Throwable],
      key: Witness.Aux[Key],
      th: Lazy[FromDynamoValue[F, H]],
      tt: Lazy[FromDynamoValue[F, T]]
  ): FromDynamoValue[F, FieldType[Key, H] :: T] = new FromDynamoValue[F, FieldType[Key, H] :: T] {
    def from(av: DynamoValue): F[labelled.FieldType[Key, H] :: T] = av match {
      case M(m) =>
        val v = m
          .get(key.value.name)
          .toRight(AttributeValueFormatException)
          .liftTo[F]
          .flatMap(th.value.from)
          .map(v => field[Key](v))
        val t = tt.value.from(av)
        (v, t).tupled.map {
          case (v, t) => v :: t
        }
      case _ => F.raiseError(BaseCaseNotPossibleException)
    }
  }

  implicit def fromAttributeValueForGeneric[F[_]: MonadError[?[_], Throwable], T, R <: HList](
      implicit gen: LabelledGeneric.Aux[T, R],
      fav: FromDynamoValue[F, R]
  ): FromDynamoValue[F, T] = new FromDynamoValue[F, T] {
    def from(av: DynamoValue): F[T] = fav.from(av).map(gen.from)
  }
}

object auto extends LowPriorityFromAttributeValue with LowPriorityToAttributeValue with AutoToAttributeValue with AutoFromAttributeValue {}
