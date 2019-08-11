package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.::
import shapeless.labelled.FieldType
import shapeless.Witness
import shapeless.HNil
import shapeless.HList
import shapeless.BasisConstraint
import shapeless.{ValueConstraint, KeyConstraint}
import shapeless.Lazy
import shapeless.ops.hlist.Prepend
import shapeless.LabelledGeneric
import shapeless.ops.record.Keys
import shapeless.NotContainsConstraint
import com.engitano.dynamof.Index._

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
        globalSecondaryIndexes: Seq[GlobalSecondaryIndex]
    )(implicit pk: IsPrimaryKey[KeyId, KeyValue]) =
      CreateTableRequest(table, pk.primaryKeyDefinition, readCapacity, writeCapacity, localSecondaryIndexes, globalSecondaryIndexes)
    def put(a: A)(implicit tdv: ToDynamoMap[A]) =
      PutItemRequest(table, tdv.to(a))
    def delete(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      DeleteItemRequest(table, k.primaryKey(h))
    def get(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      GetItemRequest[A](table, k.primaryKey(h))
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

  trait QueryableOps[A, KeyId, KeyValue] {

    val table: String
    val index: Option[String]

    def list[HK <: Symbol, HV, RK <: Symbol, RV](h: HV, startAt: Option[RV] = None)(
        implicit k: IsCompositeKey.Aux[KeyId, KeyValue, HK, HV, RK, RV]
    ) =
      ListItemsRequest[A](table, k.hashKey(h).m.head, startAt.map(rv => k.primaryKey((h, rv))))
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
        nrk: NotContainsConstraint[QK, RK]
    ) =
      QueryRequest[A](
        table,
        ck.hashKey(key).m.head,
        rangeKeyPredicate.toPredicate(wr.value.name),
        limit,
        tp.to(filterPredicate),
        startAt.map(ck.primaryKey),
        index
      )
  }
}
