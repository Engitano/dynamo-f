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
  )(implicit isCompoundKey: KeyId =:= (HKY, RKY)): UpdateOps[A, KeyId, KeyValue] =
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

  trait TableOps[A, KeyId, KeyValue] {

    val table: String

    def create(
        readCapacity: Long,
        writeCapacity: Long,
        localSecondaryIndexes: Seq[LocalSecondaryIndex],
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex],
        replicaRegions: Seq[Region] = Seq()
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      liftF(
        lift[DynamoOpA, Unit](
          CreateTableRequest(
            table,
            pk.primaryKeyDefinition,
            readCapacity,
            writeCapacity,
            localSecondaryIndexes,
            globalSecondaryIndexes,
            replicaRegions
          )
        )
      )
    def put(a: A)(implicit tdv: ToDynamoMap[A]) =
      liftF(lift[DynamoOpA, Unit](PutItemRequest(table, tdv.to(a))))
    def delete(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      liftF(lift[DynamoOpA, Unit](DeleteItemRequest(table, k.primaryKey(h))))
    def get(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue], fdv: FromDynamoValue[A]) =
      liftF(lift[DynamoOpA, Option[A]](GetItemRequest[A](table, k.primaryKey(h), fdv)))

    def describe() = liftF(lift(DescribeTableRequest(table)))
    def drop()     = liftF(lift(DeleteTableRequest(table)))

    def createP(
        readCapacity: Long,
        writeCapacity: Long,
        localSecondaryIndexes: Seq[LocalSecondaryIndex],
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex],
        replicaRegions: Seq[Region] = Seq()
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      lift[DynamoOpA, Unit](
        CreateTableRequest(
          table,
          pk.primaryKeyDefinition,
          readCapacity,
          writeCapacity,
          localSecondaryIndexes,
          globalSecondaryIndexes,
          replicaRegions
        )
      )
    def putP(a: A)(implicit tdv: ToDynamoMap[A]) =
      lift[DynamoOpA, Unit](PutItemRequest(table, tdv.to(a)))
    def deleteP(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      lift[DynamoOpA, Unit](DeleteItemRequest(table, k.primaryKey(h)))
    def getP(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue], fdv: FromDynamoValue[A]) =
      lift[DynamoOpA, Option[A]](GetItemRequest[A](table, k.primaryKey(h), fdv))

    def describeP() = lift((DescribeTableRequest(table)))
    def dropP()     = lift(DeleteTableRequest(table))
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
    def set[
        ARepr <: HList, 
        UE <: HList,
        Keyz <: HList](
        key: KeyValue,
        setExpression: UE
    )(
        implicit lg: LabelledGeneric.Aux[A, ARepr],
        k: IsPrimaryKey[KeyId, KeyValue],
        fdv: FromDynamoValue[A],
        keys: Keys.Aux[UE, Keyz],
        dontUpdateHK: NotContainsConstraint[Keyz, HK],
        dontUpdateRK: NotContainsConstraint[Keyz, RK],
        bc: BasisConstraint[UE, ARepr],
        tdv: ToDynamoMap[UE]
    ) = {
      val values = tdv.to(setExpression)
      val setValues = values.m.map(p => SetValue(p._1, p._2)).toList
      liftF(lift[DynamoOpA, Unit](UpdateItemRequest(table, k.primaryKey(key), SetExpression(SetValues(setValues)))))
    }
  }

  trait QueryableOps[A, KeyId, KeyValue] {

    val table: String
    val index: Option[String]

    def list[HK <: Symbol, HV, RK <: Symbol, RV](h: HV, startAt: Option[RV] = None)(
        implicit k: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        fdv: FromDynamoValue[A]
    ) =
      liftF(
        lift[DynamoOpA, QueryResponse[A]](
          ListItemsRequest[A](table, k.hashKey(h).m.head, startAt.map(rv => k.primaryKey((h, rv))), index, fdv)
        )
      )

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
        rangeKeyPredicate: FieldPredicate[RV],
        filterPredicate: F = HNil,
        limit: Option[Int] = None,
        startAt: Option[KeyValue] = None
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
    ) =
      liftF(
        lift[DynamoOpA, QueryResponse[A]](
          QueryRequest[A](
            table,
            ck.hashKey(key).m.head,
            rangeKeyPredicate.toPredicate(wr.value.name),
            limit,
            tp.to(filterPredicate),
            startAt.map(ck.primaryKey),
            index,
            fdv
          )
        )
      )

    def listP[HK <: Symbol, HV, RK <: Symbol, RV](h: HV, startAt: Option[RV] = None)(
        implicit k: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        fdv: FromDynamoValue[A]
    ) =
      lift[DynamoOpA, QueryResponse[A]](
        ListItemsRequest[A](table, k.hashKey(h).m.head, startAt.map(rv => k.primaryKey((h, rv))), index, fdv)
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
        rangeKeyPredicate: FieldPredicate[RV],
        filterPredicate: F = HNil,
        limit: Option[Int] = None,
        startAt: Option[KeyValue] = None
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
    ) =
      lift[DynamoOpA, QueryResponse[A]](
        QueryRequest[A](
          table,
          ck.hashKey(key).m.head,
          rangeKeyPredicate.toPredicate(wr.value.name),
          limit,
          tp.to(filterPredicate),
          startAt.map(ck.primaryKey),
          index,
          fdv
        )
      )
  }
}
