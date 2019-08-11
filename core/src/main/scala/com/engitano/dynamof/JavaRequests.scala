package com.engitano.dynamof

import cats.syntax.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.instances.option._
import software.amazon.awssdk.services.dynamodb.model.{
  PutItemRequest => JPutItemRequest,
  CreateTableRequest => JCreateTableRequest,
  GetItemRequest => JGetItemRequest,
  AttributeDefinition => JAttributeDefinition,
  ProvisionedThroughput => JProvisionedThroughput,
  DeleteItemRequest => JDeleteItemRequest,
  QueryRequest => JQueryRequest,
  LocalSecondaryIndex => JLocalSecondaryIndex,
  GlobalSecondaryIndex => JGlobalSecondaryIndex,
  KeySchemaElement,
  KeyType
}
import scala.jdk.CollectionConverters._
import cats.data.StateT
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import com.engitano.dynamof.formats.DynamoValue
import cats.data.OptionT
import cats.data.State
import cats.arrow.FunctionK
import cats.Eval

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
    val builder      = JQueryRequest.builder().tableName(req.table)
    val valueIndex   = 0
    val hashKeyExpr  = s"$hashKeyAlias = :$valueIndex"
    val valueAliases = Map(":0" -> req.key._2.toAttributeValue)
    val nameAliases  = Map(hashKeyAlias -> req.key._1)

    val ((names, vals, v), query)            = predicateToFilter(req.queryExpression).run((nameAliases, valueAliases, 1)).value
    val ((namesWithF, valsWithF, _), filter) = req.filterExpression.fold(((names, vals, v), ""))(fe => predicateToFilter(fe).run((names, vals, v)).value)

    val initBuilder = builder
      .keyConditionExpression(s"$hashKeyExpr AND $query")
      .expressionAttributeValues(valsWithF.asJava)
      .expressionAttributeNames(namesWithF.asJava)    

    val withFilter = if(filter.isEmpty()) initBuilder else initBuilder.filterExpression(filter)
    
    val withStartKey = req.startAt.fold(withFilter)(sa => withFilter.exclusiveStartKey(sa.toAttributeValue.m()))

    val withIndex = req.index.fold(withStartKey)(withStartKey.indexName)

    req.limit.fold(withIndex)(sk => withIndex.limit(sk)).build()
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

  def predicateToFilter(p: Predicate): State[(Map[String, String], Map[String, AttributeValue], Int), String] = State {
    case (names, values, variableCount) =>
      p match {
        case LessThan(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"$nameAlias < $valueAlias")
        case LessThanOrEquals(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"$nameAlias <= $valueAlias")
        case Equals(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"$nameAlias = $valueAlias")
        case GreaterThan(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"$nameAlias > $valueAlias")
        case GreaterThanOrEquals(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"$nameAlias >= $valueAlias")
        case BeginsWith(attribute, value) =>
          val valueAlias = s":var_$variableCount"
          val nameAlias  = s"#attr__${attribute}"
          val newNames   = names + (nameAlias -> attribute)
          val newVals    = values + (valueAlias -> value.toAttributeValue)
          ((newNames, newVals, variableCount + 1), s"begins_with ($nameAlias, $valueAlias)")
        case Between(attribute, lbValue, ubValue) =>
          val lbAlias   = s":var_$variableCount"
          val ubAlias   = s":var_${variableCount + 1}"
          val nameAlias = s"#attr__${attribute}"
          val newNames  = names + (nameAlias -> attribute)
          val newVals   = values + (lbAlias -> lbValue.toAttributeValue) + (ubAlias -> ubValue.toAttributeValue)
          ((newNames, newVals, variableCount + 2), s"$nameAlias BETWEEN $lbAlias AND $ubAlias")
        case And(lhs, rhs) =>
          val lhsExp = predicateToFilter(lhs)
          val rhsExp = predicateToFilter(rhs)
          val res    = (lhsExp, rhsExp).mapN((p1, p2) => s"${p1} AND ${p2}")
          res.run((names, values, variableCount)).value
      }
  }

  object NatEvalOpt extends FunctionK[Eval, Option] {
    def apply[A](fa: Eval[A]): Option[A] = fa.value.some
  }
}
