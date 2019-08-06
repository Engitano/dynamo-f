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
    def get(h: KeyValue)(implicit toKey: ToKey[KeyId, KeyValue]) =
      GetItemRequest[A](table.name, toKey.toKey(h))

    def list[HV, HK <: Symbol](h: HV)(implicit k: KeyValue <:< (HV, _), eq: KeyId <:< (HK, _), w: Witness.Aux[HK], tdv: ToDynamoValue[HV]) =
      ListItemsRequest[A](table.name, (w.value.name -> tdv.to(h)), None)
    def put(a: A)(implicit tdv: ToDynamoMap[A]) =
      PutItemRequest(table.name, tdv.to(a))
    def delete(h: KeyValue)(implicit toKey: ToKey[KeyId, KeyValue]) =
      DeleteItemRequest(table.name, toKey.toKey(h))
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
        kid: KeyId <:< (HK, RK),
        k: KeyValue <:< (HV, RV),
        tdvr: ToDynamoValue[RV],
        thk: ToKey[HK, HV],
        wr: Witness.Aux[RK],
        tp: ToPredicate[F],
        repr: LabelledGeneric.Aux[A, Repr],
        fields: Keys.Aux[Repr, AK],
        qFields: Keys.Aux[F, QK],
        ev: BasisConstraint[QK, AK],
        nhk: NotContainsConstraint[QK, HK],
        nrk: NotContainsConstraint[QK, RK],
        tk: ToKey[KeyId, KeyValue]
    ) = QueryRequest[A](table.name, thk.toKey(key).m.head, h.toPredicate(wr.value.name), limit, tp.to(predicate), startAt.map(tk.toKey))
  }
}
