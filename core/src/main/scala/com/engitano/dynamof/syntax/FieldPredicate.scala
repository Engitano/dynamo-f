package com.engitano.dynamof.syntax
import com.engitano.dynamof.formats.ToDynamoValue
import com.engitano.dynamof._
import com.engitano.dynamof.formats.DynamoValue
import com.engitano.dynamof.formats.`package`.NonEmptyString

sealed trait FieldPredicate[KV] {
    def toPredicate(key: String)(implicit tdv: ToDynamoValue[KV]): Predicate = this match {
      case lt(v)              => LessThan(key, tdv.to(v))
      case lte(v)             => LessThanOrEquals(key, tdv.to(v))
      case equalTo(v)         => Equals(key, tdv.to(v))
      case gte(v)             => GreaterThanOrEquals(key, tdv.to(v))
      case gt(v)              => GreaterThan(key, tdv.to(v))
      case between(a, b)      => Between(key, tdv.to(a), tdv.to(b))
      case bw @ beginsWith(_) => BeginsWith(key, DynamoValue.S(bw.getString))
    }
  }
  case class lt[KV](kv: KV)      extends FieldPredicate[KV]
  case class lte[KV](kv: KV)     extends FieldPredicate[KV]
  case class equalTo[KV](kv: KV) extends FieldPredicate[KV]
  case class gte[KV](kv: KV)     extends FieldPredicate[KV]
  case class gt[KV](kv: KV)      extends FieldPredicate[KV]
  case class beginsWith[KV](kv: KV)(implicit isS: KV =:= NonEmptyString) extends FieldPredicate[KV] {
    def getString: String = isS(kv).value
  }
  case class between[KV](lower: KV, upper: KV) extends FieldPredicate[KV]
  