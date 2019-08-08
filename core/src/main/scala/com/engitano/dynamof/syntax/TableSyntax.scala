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

trait TableSyntax {

  implicit def toTableOps[A, KeyId, KeyValue](table: Table.Aux[A, KeyId, KeyValue]) = new TableOps(table)

  case class FilterBuilder[A, KeyId, RK](
      table: String,
      key: (String, DynamoValue),
      queryExpression: FieldPredicate[RK],
      limit: Option[Int],
      filterExpression: Option[Predicate],
      startAt: Option[DynamoValue.M]
  )

  class TableOps[A, KeyId, KeyValue](table: Table.Aux[A, KeyId, KeyValue]) {

    def create(readCapacity: Long, writeCapacity: Long) =
      CreateTableRequest(table.name, table.key, readCapacity, writeCapacity)
    def get(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      GetItemRequest[A](table.name, k.primaryKey(h))

    def list[HV, HK <: Symbol](h: HV)(implicit k: IsCompoundKey.Aux[KeyId, KeyValue, HK, HV, _, _]) =
      ListItemsRequest[A](table.name, k.hashKey(h).m.head, None)
    def put(a: A)(implicit tdv: ToDynamoMap[A]) =
      PutItemRequest(table.name, tdv.to(a))
    def delete(h: KeyValue)(implicit k: IsPrimaryKey[KeyId, KeyValue]) =
      DeleteItemRequest(table.name, k.primaryKey(h))
    def query[
        Repr <: HList,
        HK <: Symbol,
        RK <: Symbol,
        AK <: HList,
        QK <: HList,
        HV,
        RV,
        F <: HList
    ](key: HV, h: FieldPredicate[RV], predicate: F = HNil, limit: Option[Int] = None, startAt: Option[KeyValue] = None)(
        implicit
        ck: IsCompoundKey.Aux[KeyId, KeyValue, HK, HV, RK, RV],
        tdvr: ToDynamoValue[RV],
        wr: Witness.Aux[RK],
        tp: ToPredicate[F],
        fields: FieldNames.Aux[A, AK],
        qFields: Keys.Aux[F, QK],
        ev: BasisConstraint[QK, AK],
        nhk: NotContainsConstraint[QK, HK],
        nrk: NotContainsConstraint[QK, RK]
    ) = QueryRequest[A](table.name, ck.hashKey(key).m.head, h.toPredicate(wr.value.name), limit, tp.to(predicate), startAt.map(ck.primaryKey))
  }
}
