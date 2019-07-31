/*
 * Copyright (c) 2019 Engitano
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.formats.ToDynamoValue
import cats.syntax.functor._
import cats.syntax.flatMap._
import shapeless.labelled._
import shapeless.ops.record.Selector
import shapeless.{=:!=, HList, LabelledGeneric, Witness, Generic, ::, HNil}
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import cats.effect.Async
import software.amazon.awssdk.services.dynamodb.model.{
  PutItemRequest => JPutItemRequest,
  CreateTableRequest => JCreateTableRequest,
  GetItemRequest => JGetItemRequest,
  AttributeDefinition => JAttributeDefinition,
  ProvisionedThroughput => JProvisionedThroughput
}

import scala.jdk.CollectionConverters._
import scala.jdk.FunctionConverters
import software.amazon.awssdk.services.dynamodb.model.KeySchemaElement
import software.amazon.awssdk.services.dynamodb.model.KeyType
import java.util.concurrent.CompletableFuture
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

case class CreateTableRequest(name: String, pk: PrimaryKey, readCapacity: Long, writeCapacity: Long)
case class GetItemRequest[A](table: String, key: DynamoValue.M)
case class QueryRequest[A](table: String, key: DynamoValue.M)
case class PutItemRequest(table: String, document: DynamoValue.M)

sealed trait ToKey[A] {
  def toKey(a: A): DynamoValue.M
}

object ToKey {

  implicit def toHashKey[V: ToDynamoValue] = new ToKey[(SimpleKey, V)] {
    def toKey(a: (SimpleKey, V)): DynamoValue.M = DynamoValue.M(Map(a._1.attribute.name -> ToDynamoValue[V].to(a._2)))
  }
  implicit def toHashKeyOther[H: ToDynamoValue, R: ToDynamoValue] = new ToKey[(ComplexKey, (H, R))] {
    def toKey(a: (ComplexKey, (H, R))): DynamoValue.M =
      DynamoValue.M(
        Map(
          a._1.hashKey.name  -> ToDynamoValue[H].to(a._2._1),
          a._1.rangeKey.name -> ToDynamoValue[R].to(a._2._2)
        )
      )
  }
}

case class AttributeDefinition(name: String, attrType: ScalarAttributeType)
sealed trait PrimaryKey
case class SimpleKey(attribute: AttributeDefinition)                               extends PrimaryKey
case class ComplexKey(hashKey: AttributeDefinition, rangeKey: AttributeDefinition) extends PrimaryKey

trait HasScalarAttributeRepr[A] {
  def to: ScalarAttributeType
}

object HasScalarAttributeRepr {
  implicit def scalarAttrTypeForInt = new HasScalarAttributeRepr[Int] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForLong = new HasScalarAttributeRepr[Long] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForBool = new HasScalarAttributeRepr[Boolean] {
    def to = ScalarAttributeType.B
  }
  implicit def scalarAttrTypeForNes = new HasScalarAttributeRepr[formats.NonEmptyString] {
    def to = ScalarAttributeType.S
  }
}

sealed trait Table[A] {
  val name: String
  val key: KeyId
  type KeyId
  type KeyValue
  def create(readCapacity: Long, writeCapacity: Long)(implicit hk: KeyId <:< PrimaryKey) = CreateTableRequest(this.name, hk(key), readCapacity, writeCapacity)
  def get(h: KeyValue)(implicit toKey: ToKey[(KeyId, KeyValue)]) =
    GetItemRequest[A](name, toKey.toKey((key, h)))
  def query[HK](h: HK)(implicit k: KeyValue <:< (HK, _), toKey: ToKey[(String, HK)], eq: KeyId <:< (String, _)) =
    QueryRequest[A](name, toKey.toKey(eq(key)._1 -> h))
  def put(a: A)(implicit tdv: ToDynamoMap[A]) = PutItemRequest(this.name, tdv.to(a))

}

object Table {

  type Aux[A0, KID, KV] = Table[A0] {
    type KeyId    = KID
    type KeyValue = KV
  }

  case class TableDefPartiallyApplied[A](dummy: Boolean = false) extends AnyVal {
    def apply[K <: Symbol, V, Repr <: HList](tableName: String, hk: Witness.Aux[K])(
        implicit
        gen: LabelledGeneric.Aux[A, Repr],
        s: Selector.Aux[Repr, K, V],
        sat: HasScalarAttributeRepr[V]
    ): Table.Aux[A, SimpleKey, V] = new Table[A] {
      val name           = tableName
      val key: SimpleKey = SimpleKey(AttributeDefinition(hk.value.name, sat.to))
      type KeyId    = SimpleKey
      type KeyValue = V
    }
    def apply[HK <: Symbol, HV, RK <: Symbol, RV, Repr <: HList](tableName: String, hk: Witness.Aux[HK], rk: Witness.Aux[RK])(
        implicit
        gen: LabelledGeneric.Aux[A, Repr],
        hs: Selector.Aux[Repr, HK, HV],
        hsat: HasScalarAttributeRepr[HV],
        rs: Selector.Aux[Repr, RK, RV],
        rsat: HasScalarAttributeRepr[RV]
    ): Table.Aux[A, ComplexKey, (HV, RV)] = new Table[A] {
      val name = tableName
      val key  = ComplexKey(AttributeDefinition(hk.value.name, hsat.to), AttributeDefinition(rk.value.name, rsat.to))
      type KeyId    = ComplexKey
      type KeyValue = (HV, RV)
    }
  }

  def apply[A]: TableDefPartiallyApplied[A] = TableDefPartiallyApplied[A]()

}

trait DynamoFClient[F[_]] {
  type Serializer[A]   = ToDynamoValue[A]
  type Deserializer[A] = FromDynamoValue[F, A]

  def createTable(req: CreateTableRequest): F[Unit]
  def putItem(req: PutItemRequest): F[Unit]
  def getItem[A: Deserializer](req: GetItemRequest[A]): F[A]
  def query[A: Deserializer](req: QueryRequest[A]): F[Seq[A]]

}

object DynamoFClient {
  import CompletableFutureSyntax._
  def apply[F[_]](client: DynamoDbAsyncClient)(implicit F: Async[F]): DynamoFClient[F] = new DynamoFClient[F] {
    def createTable(req: CreateTableRequest): F[Unit] =
      client
        .createTable(JavaRequests.to(req))
        .lift[F]
        .as(())

    def getItem[A](req: GetItemRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[A] =
      client
        .getItem(JavaRequests.to(req))
        .lift[F]
        .flatMap(r => fdv.from(DynamoValue.M.parse(r.item().asScala.toMap)))

    def putItem(req: PutItemRequest): F[Unit] =
      client
        .putItem(JavaRequests.to(req))
        .lift[F]
        .as(())

    def query[A](req: QueryRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[Seq[A]] = ???
  }
}

object JavaRequests {
  def to(req: PutItemRequest): JPutItemRequest =
    JPutItemRequest
      .builder()
      .tableName(req.table)
      .item(req.document.toAttributeValue.m())
      .build()

  def to(req: CreateTableRequest): JCreateTableRequest =
    JCreateTableRequest
      .builder()
      .tableName(req.name)
      .attributeDefinitions(toAttributeDefinitions(req.pk): _*)
      .keySchema(toKeySchemaDefinitions(req.pk).asJava)
      .provisionedThroughput(JProvisionedThroughput.builder().readCapacityUnits(req.readCapacity).writeCapacityUnits(req.writeCapacity).build())
      .build()

  def to(req: GetItemRequest[_]): JGetItemRequest =
    JGetItemRequest
      .builder()
      .tableName(req.table)
      .key(req.key.toAttributeValue.m())
      .build()

  def toAttributeDefinition(attr: AttributeDefinition) = attr match {
    case AttributeDefinition(name, attrType) => JAttributeDefinition.builder().attributeName(name).attributeType(attrType).build()
  }

  def toAttributeDefinitions(pk: PrimaryKey) = pk match {
    case SimpleKey(attr)    => Seq(toAttributeDefinition(attr))
    case ComplexKey(hk, rk) => Seq(toAttributeDefinition(hk), toAttributeDefinition(rk))
  }

  def buildKeySchemaElement(name: String, keyType: KeyType) = KeySchemaElement.builder().keyType(keyType).attributeName(name).build
  def toKeySchemaDefinitions(pk: PrimaryKey) = pk match {
    case SimpleKey(AttributeDefinition(name, _)) => Seq(buildKeySchemaElement(name, KeyType.HASH))
    case ComplexKey(AttributeDefinition(hk, _), AttributeDefinition(rk, _)) =>
      Seq(
        buildKeySchemaElement(hk, KeyType.HASH),
        buildKeySchemaElement(rk, KeyType.RANGE)
      )
  }
}

object CompletableFutureSyntax extends CompletableFutureSyntax

trait CompletableFutureSyntax {

  implicit def toOps[A](cf: => CompletableFuture[A]): CompletableFutureOps[A] = new CompletableFutureOps[A](cf)

  class CompletableFutureOps[A](cf: => CompletableFuture[A]) {
    def lift[F[_]](implicit F: Async[F]): F[A] = F.async { cb =>
      cf.handle[Unit] { (a, t) =>
        (Option(a), Option(t)) match {
          case (Some(a), None) => cb(Right(a))
          case (None, Some(t)) => cb(Left(t))
          case _               => cb(Left(new Exception("Impossible CompletableFuture State")))
        }
      }
    }
  }
}
