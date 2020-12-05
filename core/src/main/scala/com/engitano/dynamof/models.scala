package com.engitano.dynamof

import com.engitano.dynamof.formats._
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType
import software.amazon.awssdk.regions.Region
import com.engitano.dynamof.formats.FromDynamoValue
import software.amazon.awssdk.services.dynamodb.model.TableDescription
import cats.data.NonEmptyList
import com.engitano.dynamof.formats.DynamoValue.Bool
import cats.free.Free
import cats.free.FreeApplicative

object DynamoOp {
    def pure[A](a: A) = Free.liftF(DynamoOp.pureP(a))
    def pureP[A](a: A) = FreeApplicative.pure(a)
}

sealed trait DynamoOpA[A]
case class DescribeTableRequest(name: String) extends DynamoOpA[Option[TableDescription]]
case class CreateTableRequest(
    name: String,
    pk: PrimaryKey,
    readCapacity: Long,
    writeCapacity: Long,
    localIndexes: Seq[LocalSecondaryIndex],
    globalIndexes: Seq[GlobalSecondaryIndex],
    replicaRegions: Seq[Region]
) extends DynamoOpA[Unit]
case class DeleteTableRequest(name: String)                                              extends DynamoOpA[Unit]
case class GetItemRequest[A](table: String, key: DynamoValue.M, fdv: FromDynamoValue[A]) extends DynamoOpA[Option[A]]

sealed trait WriteItem
case class PutItemRequest(table: String, document: DynamoValue.M) extends DynamoOpA[Unit] with WriteItem
case class DeleteItemRequest(table: String, key: DynamoValue.M)   extends DynamoOpA[Unit] with WriteItem
case class UpdateItemRequest(table: String, keyExpression: DynamoValue.M, updateExpression: UpdateExpression)
    extends DynamoOpA[Unit]
    with WriteItem
case class TransactWriteRequest(operations: NonEmptyList[WriteItem]) extends DynamoOpA[Unit]
case class QueryRequest[A](
    table: String,
    key: (String, DynamoValue),
    queryExpression: Option[Predicate],
    limit: Option[Int],
    filterExpression: Option[Predicate],
    startAt: Option[DynamoValue.M],
    index: Option[String],
    fdv: FromDynamoValue[A],
    descending: Boolean
) extends DynamoOpA[QueryResponse[A]]

case class QueryResponse[A](results: List[A], lastEvaluatedKey: Option[DynamoValue.M])

case class GlobalSecondaryIndex(name: String, table: String, key: PrimaryKey, readCapacity: Long, writeCapacity: Long)
case class LocalSecondaryIndex(name: String, table: String, key: PrimaryKey)
case class AttributeDefinition(name: String, attrType: ScalarAttributeType)
sealed trait PrimaryKey
case class SimpleKey(attribute: AttributeDefinition)                                 extends PrimaryKey
case class CompositeKey(hashKey: AttributeDefinition, rangeKey: AttributeDefinition) extends PrimaryKey

sealed trait Predicate
case class LessThan(attribute: String, value: DynamoValue)                            extends Predicate
case class LessThanOrEquals(attribute: String, value: DynamoValue)                    extends Predicate
case class Equals(attribute: String, value: DynamoValue)                              extends Predicate
case class GreaterThanOrEquals(attribute: String, value: DynamoValue)                 extends Predicate
case class GreaterThan(attribute: String, value: DynamoValue)                         extends Predicate
case class Between[V <: DynamoValue](attribute: String, lowerBound: V, upperBound: V) extends Predicate
case class BeginsWith(attribute: String, value: DynamoValue.S)                        extends Predicate
case class And(lhs: Predicate, rhs: Predicate)                                        extends Predicate

// trait DynamoValueSet[A] {
//   def getSet(a: A): Set[DynamoValue]
// }

// object DynamoValueSet {
//   implicit def apply[A](implicit dvs: DynamoValueSet[A]) = dvs
//   implicit def dynamoValueSetStringSet: DynamoValueSet[DynamoValue.SS] = new DynamoValueSet[DynamoValue.SS] {
//     override def getSet(a: DynamoValue.SS): Set[DynamoValue] = a.ss.map(s => s.asInstanceOf[DynamoValue])
//   }
//   implicit def dynamoValueSetStringNumber: DynamoValueSet[DynamoValue.NS] = new DynamoValueSet[DynamoValue.NS] {
//     override def getSet(a: DynamoValue.NS): Set[DynamoValue] = a.ns.map(s => s.asInstanceOf[DynamoValue])
//   }
//   implicit def dynamoValueSetStringBinary: DynamoValueSet[DynamoValue.BS] = new DynamoValueSet[DynamoValue.BS] {
//     override def getSet(a: DynamoValue.BS): Set[DynamoValue] = a.bs.map(s => s.asInstanceOf[DynamoValue])
//   }
// }

sealed trait UpdateExpression
sealed trait SetAction
case class SetValue(attribute: String, value: DynamoValue, ifNotExists: Boolean = false)
case class SetValues(operations: List[SetValue])                                      extends SetAction
case class IncrementValue(attribute: String, value: DynamoValue.N, subtract: Boolean) extends SetAction
// case class AppendValues(attribute: String, value: DynamoValue.L)                            extends SetAction
case class SetExpression(action: SetAction) extends UpdateExpression
// case class RemoveExpression(attributesToRemove: Set[String])                                extends UpdateExpression
// case class DeleteExpression[V <: DynamoValue: DynamoValueSet](attribute: String, values: V) extends UpdateExpression
