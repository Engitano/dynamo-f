package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._


trait TableSyntax {

  implicit def toTableOps[A, KeyId, KeyValue](table: Table.Aux[A, KeyId, KeyValue]) = new TableOps(table)

  class TableOps[A, KeyId, KeyValue](table: Table.Aux[A, KeyId, KeyValue]) {

    def create(readCapacity: Long, writeCapacity: Long)(implicit hk: KeyId <:< PrimaryKey) =
      CreateTableRequest(table.name, hk(table.key), readCapacity, writeCapacity)
    def get(h: KeyValue)(implicit toKey: ToKey[(KeyId, KeyValue)]) =
      GetItemRequest[A](table.name, toKey.toKey((table.key, h)))
    def list[HK](h: HK)(implicit k: KeyValue <:< (HK, _), tdv: ToDynamoValue[HK], eq: KeyId =:= CompositeKey) =
      ListItemsRequest[A](table.name, (eq(table.key).hashKey.name -> tdv.to(h)), None)
    def put(a: A)(implicit tdv: ToDynamoMap[A]) = 
      PutItemRequest(table.name, tdv.to(a))
    def delete(h: KeyValue)(implicit toKey: ToKey[(KeyId, KeyValue)]) =
      DeleteItemRequest(table.name, toKey.toKey((table.key, h)))
    
    def query[HK, RK](key: HK, h: KeyPredicate[RK])
      (implicit k: KeyValue <:< (HK, RK), tdvh: ToDynamoValue[HK], tdvr: ToDynamoValue[RK], eq: KeyId =:= CompositeKey) = {
      QueryRequest[A](table.name, eq(table.key).hashKey.name -> tdvh.to(key), h.toPredicate(eq(table.key).rangeKey.name), None, None, None)
    }
  }
}

sealed trait KeyPredicate[KV] {
  def toPredicate(key: String)(implicit tdv: ToDynamoValue[KV]): Predicate = this match {
    case lt(v) => LessThan(key, tdv.to(v))
    case lte(v) => LessThanOrEquals(key, tdv.to(v))
    case equalTo(v) => Equals(key, tdv.to(v))
    case gte(v) => GreaterThanOrEquals(key, tdv.to(v))
    case gt(v) => GreaterThan(key, tdv.to(v))
    case between(a,b) => Between(key, tdv.to(a), tdv.to(b))
  }
}
case class lt[KV](kv: KV) extends KeyPredicate[KV]
case class lte[KV](kv: KV) extends KeyPredicate[KV]
case class equalTo[KV](kv: KV) extends KeyPredicate[KV]
case class gte[KV](kv: KV) extends KeyPredicate[KV]
case class gt[KV](kv: KV) extends KeyPredicate[KV]
case class between[KV](lower: KV, upper: KV) extends KeyPredicate[KV]

final case class QueryBuilder[A, Range](keyExpression: Option[KeyPredicate[Range]]){
  def withKeyExpression(predcate: KeyPredicate[Range]) = this.copy(keyExpression = Some(predcate))
}