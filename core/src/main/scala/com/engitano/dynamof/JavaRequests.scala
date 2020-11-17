package com.engitano.dynamof

import cats.syntax.option._
import cats.syntax.apply._
import software.amazon.awssdk.services.dynamodb.model.{
  PutItemRequest => JPutItemRequest,
  CreateTableRequest => JCreateTableRequest,
  GetItemRequest => JGetItemRequest,
  UpdateItemRequest => JUpdateItemRequest,
  AttributeDefinition => JAttributeDefinition,
  ProvisionedThroughput => JProvisionedThroughput,
  DeleteItemRequest => JDeleteItemRequest,
  QueryRequest => JQueryRequest,
  LocalSecondaryIndex => JLocalSecondaryIndex,
  GlobalSecondaryIndex => JGlobalSecondaryIndex,
  Put => JPut,
  Update => JUpdate,
  Delete => JDelete,
  Get => JGet,
  TransactWriteItem,
  KeySchemaElement,
  KeyType
}
import scala.jdk.CollectionConverters._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import cats.data.State
import cats.arrow.FunctionK
import cats.Eval
import software.amazon.awssdk.services.dynamodb.model.Projection
import software.amazon.awssdk.services.dynamodb.model.ProjectionType
import com.engitano.dynamof.formats.DynamoValue
import software.amazon.awssdk.services.dynamodb.model.TransactWriteItemsRequest

private object JavaRequests {

  def to(req: PutItemRequest): JPutItemRequest =
    JPutItemRequest
      .builder()
      .tableName(req.table)
      .item(req.document.toAttributeValue.m())
      .build()

  def to(req: TransactWriteRequest): TransactWriteItemsRequest = {
    val items = req.operations.map {
      case r: PutItemRequest    => TransactWriteItem.builder.put(JavaRequests.toWriteItem(r)).build()
      case r: UpdateItemRequest => TransactWriteItem.builder.update(JavaRequests.toWriteItem(r)).build()
      case r: DeleteItemRequest => TransactWriteItem.builder.delete(JavaRequests.toWriteItem(r)).build()
    }
    return TransactWriteItemsRequest.builder().transactItems(items.toList.asJava).build()
  }

  def toWriteItem(req: PutItemRequest): JPut =
    JPut
      .builder()
      .tableName(req.table)
      .item(req.document.toAttributeValue.m())
      .build()

  def to(req: CreateTableRequest): JCreateTableRequest = {
    val builder = JCreateTableRequest
      .builder()
      .tableName(req.name)
      .attributeDefinitions(toAttributeDefinitions(Seq(req.pk) ++ req.globalIndexes.map(_.key) ++ req.localIndexes.map(_.key): _*): _*)
      .keySchema(toKeySchemaDefinitions(req.pk).asJava)
      .provisionedThroughput(
        JProvisionedThroughput.builder().readCapacityUnits(req.readCapacity).writeCapacityUnits(req.writeCapacity).build()
      )

    val withLocalIndexes =
      if (req.localIndexes.isEmpty) builder
      else
        builder
          .localSecondaryIndexes(
            req.localIndexes
              .map(
                ix =>
                  JLocalSecondaryIndex
                    .builder()
                    .indexName(ix.name)
                    .projection(Projection.builder().projectionType(ProjectionType.ALL).build())
                    .keySchema(toKeySchemaDefinitions(ix.key).asJava)
                    .build()
              )
              .asJava
          )
    val withGlobalIndexes =
      if (req.globalIndexes.isEmpty) withLocalIndexes
      else
        withLocalIndexes.globalSecondaryIndexes(
          req.globalIndexes
            .map(
              ix =>
                JGlobalSecondaryIndex
                  .builder()
                  .indexName(ix.name)
                  .keySchema(toKeySchemaDefinitions(ix.key).asJava)
                  .projection(Projection.builder().projectionType(ProjectionType.ALL).build())
                  .provisionedThroughput(
                    JProvisionedThroughput.builder().readCapacityUnits(ix.readCapacity).writeCapacityUnits(ix.writeCapacity).build()
                  )
                  .build()
            )
            .asJava
        )

    withGlobalIndexes.build()
  }

  def to(req: UpdateItemRequest): JUpdateItemRequest = {
    val jreq     = JUpdateItemRequest.builder().tableName(req.table)
    val withKeys = jreq.key(req.keyExpression.toAttributeValue.m())
    req.updateExpression match {
      case SetExpression(SetValues(xs)) =>
        val (setExpression, expressionAttributeValues, expressionAttributeNames) =
          xs.foldLeft((List[String](), Map[String, AttributeValue](), Map[String, String]())) { (acc, n) =>
            val nextValue = s":${acc._2.size}"
            val nextName  = s"#${acc._2.size}"
            (acc._1 :+ (s"${nextName} = $nextValue"), acc._2 + (nextValue -> n.value.toAttributeValue), acc._3 + (nextName -> n.attribute))
          }

        withKeys
          .updateExpression("SET " + setExpression.mkString(", "))
          .expressionAttributeValues(expressionAttributeValues.asJava)
          .expressionAttributeNames(expressionAttributeNames.asJava)
          .build()
      case SetExpression(IncrementValue(attribute, newValue, subtract)) =>
        val attName = "#att"
        val valName = ":val"
        val exprAttributeNames = Map(attName -> attribute)
        val exprAttributeValues = Map(valName -> newValue.toAttributeValue)
        val setExpression = s"SET $attName = $attName ${ if(subtract) "-" else "+" } $valName"
        withKeys
          .updateExpression(setExpression)
          .expressionAttributeValues(exprAttributeValues.asJava)
          .expressionAttributeNames(exprAttributeNames.asJava)
          .build()
    }
  }

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

  def toWriteItem(req: DeleteItemRequest): JDelete =
    JDelete
      .builder()
      .tableName(req.table)
      .key(req.key.toAttributeValue.m())
      .build()

  def toWriteItem(req: UpdateItemRequest): JUpdate = {
    val jreq     = JUpdate.builder().tableName(req.table)
    val withKeys = jreq.key(req.keyExpression.toAttributeValue.m())
    req.updateExpression match {
      case SetExpression(SetValues(xs)) =>
        val (setExpression, expressionAttributeValues, expressionAttributeNames) =
          xs.foldLeft((List[String](), Map[String, AttributeValue](), Map[String, String]())) { (acc, n) =>
            val nextValue = s":${acc._2.size}"
            val nextName  = s"#${acc._2.size}"
            (acc._1 :+ (s"${nextName} = $nextValue"), acc._2 + (nextValue -> n.value.toAttributeValue), acc._3 + (nextName -> n.attribute))
          }

        withKeys
          .updateExpression("SET " + setExpression.mkString(", "))
          .expressionAttributeValues(expressionAttributeValues.asJava)
          .expressionAttributeNames(expressionAttributeNames.asJava)
          .build()
      case SetExpression(IncrementValue(attribute, newValue, subtract)) =>
        val attName = "#att"
        val valName = ":val"
        val exprAttributeNames = Map(attName -> attribute)
        val exprAttributeValues = Map(valName -> newValue.toAttributeValue)
        val setExpression = s"SET $attName = $attName ${ if(subtract) "-" else "+" } $valName"
        withKeys
          .updateExpression(setExpression)
          .expressionAttributeValues(exprAttributeValues.asJava)
          .expressionAttributeNames(exprAttributeNames.asJava)
          .build()
    }
  }

  def to(req: ListItemsRequest[_]): JQueryRequest = {
    val builder = JQueryRequest
      .builder()
      .tableName(req.table)
      .keyConditionExpression(s"#hk=:hk")
      .expressionAttributeNames(Map("#hk" -> req.key._1).asJava)
      .expressionAttributeValues(Map(":hk" -> req.key._2.toAttributeValue).asJava)
    val withIndex = req.index.fold(builder)(builder.indexName)
    withIndex.build()
  }

  def to(req: QueryRequest[_]): JQueryRequest = {
    val hashKeyAlias = "#hashKey"
    val builder      = JQueryRequest.builder().tableName(req.table)
    val valueIndex   = 0
    val hashKeyExpr  = s"$hashKeyAlias = :$valueIndex"
    val valueAliases = Map(":0" -> req.key._2.toAttributeValue)
    val nameAliases  = Map(hashKeyAlias -> req.key._1)

    val ((names, vals, v), query) = predicateToFilter(req.queryExpression).run((nameAliases, valueAliases, 1)).value
    val ((namesWithF, valsWithF, _), filter) =
      req.filterExpression.fold(((names, vals, v), ""))(fe => predicateToFilter(fe).run((names, vals, v)).value)

    val initBuilder = builder
      .keyConditionExpression(s"$hashKeyExpr AND $query")
      .expressionAttributeValues(valsWithF.asJava)
      .expressionAttributeNames(namesWithF.asJava)

    val withFilter = if (filter.isEmpty()) initBuilder else initBuilder.filterExpression(filter)

    val withStartKey = req.startAt.fold(withFilter)(sa => withFilter.exclusiveStartKey(sa.toAttributeValue.m()))

    val withIndex = req.index.fold(withStartKey)(withStartKey.indexName).scanIndexForward(req.ascending)

    req.limit.fold(withIndex)(sk => withIndex.limit(sk)).build()
  }

  def toAttributeDefinition(attr: AttributeDefinition) = attr match {
    case AttributeDefinition(name, attrType) => JAttributeDefinition.builder().attributeName(name).attributeType(attrType).build()
  }

  def toAttributeDefinitions(pk: PrimaryKey*) =
    pk.flatMap {
        case SimpleKey(attr)      => Seq(toAttributeDefinition(attr))
        case CompositeKey(hk, rk) => Seq(toAttributeDefinition(hk), toAttributeDefinition(rk))
      }
      .groupBy(_.attributeName())
      .map(_._2.head)
      .toSeq

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
