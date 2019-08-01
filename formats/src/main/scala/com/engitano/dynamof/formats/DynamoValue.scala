package com.engitano.dynamof.formats

import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import java.nio.ByteBuffer
import shapeless.LabelledGeneric._

import scala.jdk.CollectionConverters._
import software.amazon.awssdk.services.dynamodb.model.PutItemRequest
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

object ToDynamoValue {
  def apply[A](implicit tav: ToDynamoValue[A]) = tav
}
trait ToDynamoValue[A] {
  def to(a: A): DynamoValue
}

object ToDynamoMap {
  def apply[A](implicit tdm: ToDynamoMap[A]) = tdm
}

trait ToDynamoMap[A] {
  def to(a: A): DynamoValue.M
}

object FromDynamoValue {
  def apply[F[_], A](implicit fav: FromDynamoValue[F, A]) = fav
}
trait FromDynamoValue[F[_], A] {
  def from(av: DynamoValue): F[A]
}

trait ToDocument[F[_], T] {
  def to(t: T): F[DynamoDocument]
}

trait FromDocument[F[_], T] {
  def from(doc: DynamoDocument): F[T]
}

object DynamoValue {
  case object Empty                         extends DynamoValue
  case object Null                          extends DynamoValue
  case class Bool(b: Boolean)               extends DynamoValue
  case class S(s: String)                   extends DynamoValue
  case class N(n: String)                   extends DynamoValue
  case class B(b: ByteBuffer)               extends DynamoValue
  case class SS(ss: Set[S])                 extends DynamoValue
  case class NS(ns: Set[N])                 extends DynamoValue
  case class BS(bs: Set[B])                 extends DynamoValue
  case class L(l: List[DynamoValue])        extends DynamoValue
  case class M(m: Map[String, DynamoValue]) extends DynamoValue

  def parse(a: AttributeValue): DynamoValue = {
    if (a.nul()) Null
    else if (Option(a.bool()).isDefined) Bool(a.bool())
    else if (Option(a.s).isDefined) S(a.s)
    else if (Option(a.n).isDefined) N(a.n)
    else if (Option(a.b).isDefined) B(a.b.asByteBuffer)
    else if (!a.ss.isEmpty) SS(a.ss.asScala.toSet.map(S(_)))
    else if (!a.ns.isEmpty) NS(a.ns.asScala.toSet.map(N(_)))
    else if (!a.bs.isEmpty) BS(a.bs.asScala.map(b => B(b.asByteBuffer)).toSet)
    else if (!a.l.isEmpty) L(a.l.asScala.map(parse).toList)
    else if (!a.m.isEmpty()) M(a.m.asScala.mapValues(parse).toMap)
    else DynamoValue.Empty
  }

  object M {
    def parse(m: Map[String, AttributeValue]) : M = M(m.mapValues(v => DynamoValue.parse(v)).toMap)
  }
}

sealed abstract class DynamoValue {
  import DynamoValue._

  def attributeType: ScalarAttributeType = this match {
    case _:B => ScalarAttributeType.B
    case _:N => ScalarAttributeType.N
    case _:S => ScalarAttributeType.S
    case _ => ScalarAttributeType.UNKNOWN_TO_SDK_VERSION
  }
  
  def toAttributeValue: AttributeValue = this match {
    case Null    => AttributeValue.builder.nul(true).build()
    case Bool(b) => AttributeValue.builder.bool(b).build()
    case S(s)    => AttributeValue.builder.s(s).build()
    case N(n)    => AttributeValue.builder.n(n).build()
    case B(b)    => AttributeValue.builder.b(SdkBytes.fromByteBuffer(b)).build()
    case SS(ss)  => AttributeValue.builder.ss(ss.map(_.s).asJavaCollection).build()
    case NS(ns)  => AttributeValue.builder.ns(ns.map(_.n).asJavaCollection).build()
    case BS(bs)  => AttributeValue.builder.bs(bs.map(b => SdkBytes.fromByteBuffer(b.b)).asJavaCollection).build()
    case L(l)    => AttributeValue.builder.l(l.map(_.toAttributeValue).asJavaCollection).build()
    case M(m)    => AttributeValue.builder.m(m.view.mapValues(_.toAttributeValue).toMap.asJava).build()
    case Empty   => AttributeValue.builder.build()
  }
}
