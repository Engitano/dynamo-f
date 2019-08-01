package com.engitano.dynamof

import cats.syntax.option._
import software.amazon.awssdk.services.dynamodb.model.{
  PutItemRequest => JPutItemRequest,
  CreateTableRequest => JCreateTableRequest,
  GetItemRequest => JGetItemRequest,
  AttributeDefinition => JAttributeDefinition,
  ProvisionedThroughput => JProvisionedThroughput,
  DeleteItemRequest => JDeleteItemRequest,
  QueryRequest => JQueryRequest,
  KeySchemaElement,
  KeyType
}
import scala.jdk.CollectionConverters._
import cats.data.State
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

private object JavaRequests {
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

  def to(req: DeleteItemRequest): JDeleteItemRequest =
    JDeleteItemRequest
      .builder()
      .tableName(req.table)
      .key(req.key.toAttributeValue.m())
      .build()

  def to(req: ListItemsRequest[_]): JQueryRequest =
    JQueryRequest
      .builder()
      .tableName(req.table)
      .keyConditionExpression(s"#hk=:hk")
      .expressionAttributeNames(Map("#hk" -> req.key._1).asJava)
      .expressionAttributeValues(Map(":hk" -> req.key._2.toAttributeValue).asJava)
      .build()

  def to(req: QueryRequest[_]): JQueryRequest = {
    val hashKeyAlias = "#hashKey"
    val rangeKeyAlias        = "#rangeKey"

    def toAlias(i: Int) = s":$i"
    def to(req: Predicate, valueExpressions: Map[String, AttributeValue], ix: Int): (String, String, Map[String, AttributeValue]) = req match {
      case LessThan(attribute, value)            => (attribute, s"$rangeKeyAlias < ${toAlias(ix)}", valueExpressions + (toAlias(ix)  -> value.toAttributeValue))
      case LessThanOrEquals(attribute, value)    => (attribute, s"$rangeKeyAlias <= ${toAlias(ix)}", valueExpressions + (toAlias(ix) -> value.toAttributeValue))
      case Equals(attribute, value)              => (attribute, s"$rangeKeyAlias = ${toAlias(ix)}", valueExpressions + (toAlias(ix) -> value.toAttributeValue))
      case GreaterThanOrEquals(attribute, value) => (attribute, s"$rangeKeyAlias >= ${toAlias(ix)}", valueExpressions + (toAlias(ix) -> value.toAttributeValue))
      case GreaterThan(attribute, value)         => (attribute, s"$rangeKeyAlias > ${toAlias(ix)}", valueExpressions + (toAlias(ix)  -> value.toAttributeValue))
      case Between(attribute, lb, ub) =>
        (
          attribute,
          s"$rangeKeyAlias BETWEEN ${toAlias(ix)} AND ${toAlias(ix + 1)}",
          valueExpressions + (toAlias(ix) -> lb.toAttributeValue) + (toAlias(ix + 1) -> ub.toAttributeValue)
        )
      case And(lhs, rhs) => 
        val l = to(lhs, valueExpressions, ix) 
        val r = to(rhs, valueExpressions, ix + 1)
        (l._1, s"${l._2} AND ${r._2}", l._3 ++ r._3)        
    }
    val builder = JQueryRequest.builder().tableName(req.table)
    val valueIndex = 0
    val hashKeyExpr = s"$hashKeyAlias = :$valueIndex"
    val valueAliases = Map(":0" -> req.key._2.toAttributeValue) 
    val filterExp = to(req.queryExpression, valueAliases, valueIndex + 1)
    val nameAliases = Map(hashKeyAlias -> req.key._1, rangeKeyAlias -> filterExp._1)
    builder
      .keyConditionExpression(s"$hashKeyExpr AND ${filterExp._2}")
      .expressionAttributeValues(filterExp._3.asJava)
      .expressionAttributeNames(nameAliases.asJava)
      .build()
    
  }

  def toAttributeDefinition(attr: AttributeDefinition) = attr match {
    case AttributeDefinition(name, attrType) => JAttributeDefinition.builder().attributeName(name).attributeType(attrType).build()
  }

  def toAttributeDefinitions(pk: PrimaryKey) = pk match {
    case SimpleKey(attr)      => Seq(toAttributeDefinition(attr))
    case CompositeKey(hk, rk) => Seq(toAttributeDefinition(hk), toAttributeDefinition(rk))
  }

  def buildKeySchemaElement(name: String, keyType: KeyType) = KeySchemaElement.builder().keyType(keyType).attributeName(name).build
  def toKeySchemaDefinitions(pk: PrimaryKey) = pk match {
    case SimpleKey(AttributeDefinition(name, _)) => Seq(buildKeySchemaElement(name, KeyType.HASH))
    case CompositeKey(AttributeDefinition(hk, _), AttributeDefinition(rk, _)) =>
      Seq(
        buildKeySchemaElement(hk, KeyType.HASH),
        buildKeySchemaElement(rk, KeyType.RANGE)
      )
  }
}
