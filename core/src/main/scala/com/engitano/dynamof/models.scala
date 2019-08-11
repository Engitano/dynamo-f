package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.syntax._
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

case class CreateTableRequest(
    name: String,
    pk: PrimaryKey,
    readCapacity: Long,
    writeCapacity: Long,
    localIndexes: Seq[LocalSecondaryIndex],
    globalIndexes: Seq[GlobalSecondaryIndex]
)
case class GetItemRequest[A](table: String, key: DynamoValue.M)
case class ListItemsRequest[A](table: String, key: (String, DynamoValue), startAt: Option[DynamoValue.M])
case class PutItemRequest(table: String, document: DynamoValue.M)
case class DeleteItemRequest(table: String, key: DynamoValue.M)
case class QueryRequest[A](
    table: String,
    key: (String, DynamoValue),
    queryExpression: Predicate,
    limit: Option[Int],
    filterExpression: Option[Predicate],
    startAt: Option[DynamoValue.M],
    index: Option[String]
)
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
