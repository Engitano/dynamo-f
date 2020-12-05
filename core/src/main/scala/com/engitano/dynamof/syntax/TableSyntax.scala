package com.engitano.dynamof.syntax

import cats.free.Free.liftF
import cats.free.FreeApplicative.lift
import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.Witness
import shapeless._
import shapeless.HList
import shapeless.<:!<
import shapeless.BasisConstraint
import shapeless.ops.record.Keys
import shapeless.NotContainsConstraint
import com.engitano.dynamof.Index._
import software.amazon.awssdk.regions.Region
import shapeless.ops.record.Values
import shapeless.LabelledGeneric
import shapeless.LUBConstraint
import com.fasterxml.jackson.annotation.JacksonInject.Value
import software.amazon.awssdk.services.dynamodb.model.TransactWriteItem
import cats.data.NonEmptyList
import shapeless.ops.record.Selector

trait TableSyntax {

  implicit def toTableOps[A, KeyId, KeyValue](
      tbl: Table.Aux[A, KeyId, KeyValue]
  ): TableOps[A, KeyId, KeyValue] with QueryableOps[A, KeyId, KeyValue] =
    new TableOps[A, KeyId, KeyValue] with QueryableOps[A, KeyId, KeyValue] {
      val table = tbl.name
      val index = None
    }

  implicit def toIndexOps[A, KeyId, KeyValue, IXT <: IndexType](
      ix: Index.Aux[A, KeyId, KeyValue, IXT]
  ): IndexOps[A, KeyId, KeyValue, IXT] with QueryableOps[A, KeyId, KeyValue] =
    new IndexOps[A, KeyId, KeyValue, IXT] with QueryableOps[A, KeyId, KeyValue] {
      val table = ix.tableName
      val name  = ix.indexName
      val index = Some(ix.indexName)
    }

  implicit def toUpdateOpsSimpleKey[A, KeyId <: Symbol, KeyValue](
      tbl: Table.Aux[A, KeyId, KeyValue]
  ): UpdateOps[A, KeyId, KeyValue] =
    new UpdateOps[A, KeyId, KeyValue] {
      val table = tbl.name
      type HK = KeyId
    }

  implicit def toUpdateOpsCompoundKey[A, KeyId, KeyValue, HKY <: Symbol, RKY <: Symbol](
      tbl: Table.Aux[A, KeyId, KeyValue]
  ): UpdateOps[A, KeyId, KeyValue] =
    new UpdateOps[A, KeyId, KeyValue] {
      val table = tbl.name
      type HK = HKY
      type RK = RKY
    }

  case class FilterBuilder[A, KeyId, RK](
      table: String,
      key: (String, DynamoValue),
      queryExpression: FieldPredicate[RK],
      limit: Option[Int],
      filterExpression: Option[Predicate],
      startAt: Option[DynamoValue.M]
  )

  trait KeyOps[KeyId, KeyValue] {
    def parseKey(key: DynamoValue.M)(implicit ipk: IsPrimaryKey[KeyId, KeyValue]): Either[DynamoUnmarshallException, KeyValue] = ipk.parseKey(key)
  }

  trait TableOps[A, KeyId, KeyValue] extends KeyOps[KeyId, KeyValue] {

    val table: String    

    def createOp(
        readCapacity: Long,
        writeCapacity: Long,
        localSecondaryIndexes: Seq[LocalSecondaryIndex],
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex],
        replicaRegions: Seq[Region] = Seq()
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      CreateTableRequest(
        table,
        pk.primaryKeyDefinition,
        readCapacity,
        writeCapacity,
        localSecondaryIndexes,
        globalSecondaryIndexes,
        replicaRegions
      )
    def createP(
        readCapacity: Long,
        writeCapacity: Long,
        localSecondaryIndexes: Seq[LocalSecondaryIndex],
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex],
        replicaRegions: Seq[Region] = Seq()
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      lift[DynamoOpA, Unit](
        createOp(
          readCapacity,
          writeCapacity,
          localSecondaryIndexes,
          globalSecondaryIndexes,
          replicaRegions
        )
      )
    def create(
        readCapacity: Long,
        writeCapacity: Long,
        localSecondaryIndexes: Seq[LocalSecondaryIndex],
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex],
        replicaRegions: Seq[Region] = Seq()
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      liftF(
        createP(
          readCapacity,
          writeCapacity,
          localSecondaryIndexes,
          globalSecondaryIndexes,
          replicaRegions
        )
      )

    def putOp(a: A)(implicit tdv: ToDynamoMap[A]) = PutItemRequest(table, tdv.to(a))
    def putP(a: A)(implicit tdv: ToDynamoMap[A])  = lift[DynamoOpA, Unit](putOp(a))
    def put(a: A)(implicit tdv: ToDynamoMap[A])   = liftF(putP(a))

    def deleteOp(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) = DeleteItemRequest(table, k.primaryKey(h))
    def deleteP(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue])  = lift[DynamoOpA, Unit](deleteOp(h))
    def delete(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue])   = liftF(deleteP(h))

    def getOp(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue], fdv: FromDynamoValue[A]) =
      GetItemRequest[A](table, k.primaryKey(h), fdv)
    def getP(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue], fdv: FromDynamoValue[A]) = lift[DynamoOpA, Option[A]](getOp(h))
    def get(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue], fdv: FromDynamoValue[A])  = liftF(getP(h))

    def describeOp() = DescribeTableRequest(table)
    def describeP()  = lift(describeOp())
    def describe()   = liftF(describeP())

    def dropOp() = DeleteTableRequest(table)
    def dropP()  = lift(dropOp())
    def drop()   = liftF(dropP())
  }

  trait IndexOps[A, KeyId, KeyValue, IXT <: IndexType] {
    val table: String
    val name: String

    def definition(readCapacity: Long, writeCapacity: Long)(
        implicit
        isGlobal: IXT =:= Global,
        ipk: IsCompositeKey[KeyId, KeyValue]
    ) = GlobalSecondaryIndex(name, table, ipk.primaryKeyDefinition, readCapacity, writeCapacity)
    def definition(
        implicit
        isGlobal: IXT =:= Local,
        ipk: IsCompositeKey[KeyId, KeyValue]
    ) = LocalSecondaryIndex(name, table, ipk.primaryKeyDefinition)
  }

  trait UpdateOps[A, KeyId, KeyValue] {
    val table: String
    type HK <: Symbol
    type RK <: Symbol

    def setOp[ARepr <: HList, UE <: HList, Keyz <: HList](
        key: KeyValue,
        setExpression: UE
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        keys: Keys.Aux[UE, Keyz],
        dontUpdateHK: NotContainsConstraint[Keyz, HK],
        dontUpdateRK: NotContainsConstraint[Keyz, RK],
        bc: BasisConstraint[UE, ARepr],
        tdv: ToDynamoMap[UE]
    ) = {
      val values    = tdv.to(setExpression)
      val setValues = values.m.map(p => SetValue(p._1, p._2)).toList
      UpdateItemRequest(table, k.primaryKey(key), SetExpression(SetValues(setValues)))
    }

    def setP[ARepr <: HList, UE <: HList, Keyz <: HList](
        key: KeyValue,
        setExpression: UE
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        keys: Keys.Aux[UE, Keyz],
        dontUpdateHK: NotContainsConstraint[Keyz, HK],
        dontUpdateRK: NotContainsConstraint[Keyz, RK],
        bc: BasisConstraint[UE, ARepr],
        tdv: ToDynamoMap[UE]
    ) = lift[DynamoOpA, Unit](setOp(key, setExpression))

    def set[ARepr <: HList, UE <: HList, Keyz <: HList](
        key: KeyValue,
        setExpression: UE
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        keys: Keys.Aux[UE, Keyz],
        dontUpdateHK: NotContainsConstraint[Keyz, HK],
        dontUpdateRK: NotContainsConstraint[Keyz, RK],
        bc: BasisConstraint[UE, ARepr],
        tdv: ToDynamoMap[UE]
    ) = liftF(setP(key, setExpression))

    def incrementOp[ARepr <: HList, K <: Symbol, V](
        key: KeyValue,
        attributeToSet: Witness.Aux[K],
        incrementBy: V
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        value: Selector.Aux[ARepr, K, V],
        isNumber: Numeric[V],
        dontUpdateHK: HK =:!= K,
        dontUpdateRK: RK =:!= K,
    ) = {
      val isNegative = isNumber.toDouble(incrementBy) < 0
      val numVal = incrementBy.toString()
      UpdateItemRequest(table, k.primaryKey(key), SetExpression(IncrementValue(attributeToSet.value.name, DynamoValue.N(numVal), isNegative)))
    }

    def incrementP[ARepr <: HList, K <: Symbol, V](
        key: KeyValue,
        attributeToSet: Witness.Aux[K],
        incrementBy: V
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        value: Selector.Aux[ARepr, K, V],
        isNumber: Numeric[V],
        dontUpdateHK: HK =:!= K,
        dontUpdateRK: RK =:!= K,
    ) = lift[DynamoOpA, Unit](incrementOp(key, attributeToSet, incrementBy))

    def increment[ARepr <: HList, K <: Symbol, V](
        key: KeyValue,
        attributeToSet: Witness.Aux[K],
        incrementBy: V
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        value: Selector.Aux[ARepr, K, V],
        isNumber: Numeric[V],
        dontUpdateHK: HK =:!= K,
        dontUpdateRK: RK =:!= K,
    ) = liftF(incrementP(key, attributeToSet, incrementBy))
  }

  trait QueryableOps[A, KeyId, KeyValue] extends KeyOps[KeyId, KeyValue]  {

    val table: String
    val index: Option[String]

    def queryOp[
        HK <: Symbol,
        RK <: Symbol,
        AK <: HList,
        QK <: HList,
        HV,
        RV,
        F <: HList
    ](
        key: HV,
        rangeKeyPredicate: Option[FieldPredicate[RV]] = None,
        filterPredicate: F = HNil,
        limit: Option[Int] = None,
        startAt: Option[KeyValue] = None,
        descending: Boolean = false,
    )(
        implicit
        ck: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        tdvr: ToDynamoValue[RV],
        wr: Witness.Aux[RK],
        tp: ToPredicate[F],
        fields: FieldNames.Aux[A, AK],
        qFields: Keys.Aux[F, QK],
        ev: BasisConstraint[QK, AK],
        nhk: NotContainsConstraint[QK, HK],
        nrk: NotContainsConstraint[QK, RK],
        fdv: FromDynamoValue[A]
    ) = QueryRequest[A](
      table,
      ck.hashKey(key).m.head,
      rangeKeyPredicate.map(_.toPredicate(wr.value.name)),
      limit,
      tp.to(filterPredicate),
      startAt.map(ck.primaryKey),
      index,
      fdv,
      descending
    )

    def queryP[
        HK <: Symbol,
        RK <: Symbol,
        AK <: HList,
        QK <: HList,
        HV,
        RV,
        F <: HList
    ](
        key: HV,
        rangeKeyPredicate: Option[FieldPredicate[RV]] = None,
        filterPredicate: F = HNil,
        limit: Option[Int] = None,
        startAt: Option[KeyValue] = None,
        descending: Boolean = false
    )(
        implicit
        ck: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        tdvr: ToDynamoValue[RV],
        wr: Witness.Aux[RK],
        tp: ToPredicate[F],
        fields: FieldNames.Aux[A, AK],
        qFields: Keys.Aux[F, QK],
        ev: BasisConstraint[QK, AK],
        nhk: NotContainsConstraint[QK, HK],
        nrk: NotContainsConstraint[QK, RK],
        fdv: FromDynamoValue[A]
    ) = lift[DynamoOpA, QueryResponse[A]](queryOp(key, rangeKeyPredicate, filterPredicate, limit, startAt, descending))

    def query[
        HK <: Symbol,
        RK <: Symbol,
        AK <: HList,
        QK <: HList,
        HV,
        RV,
        F <: HList
    ](
        key: HV,
        rangeKeyPredicate: Option[FieldPredicate[RV]] = None,
        filterPredicate: F = HNil,
        limit: Option[Int] = None,
        startAt: Option[KeyValue] = None,
        descending: Boolean = false
    )(
        implicit
        ck: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        tdvr: ToDynamoValue[RV],
        wr: Witness.Aux[RK],
        tp: ToPredicate[F],
        fields: FieldNames.Aux[A, AK],
        qFields: Keys.Aux[F, QK],
        ev: BasisConstraint[QK, AK],
        nhk: NotContainsConstraint[QK, HK],
        nrk: NotContainsConstraint[QK, RK],
        fdv: FromDynamoValue[A]
    ) = liftF(queryP(key, rangeKeyPredicate, filterPredicate, limit, startAt, descending))
  }
}
