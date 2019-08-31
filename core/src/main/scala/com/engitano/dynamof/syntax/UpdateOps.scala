package com.engitano.dynamof.syntax
import com.engitano.dynamof.formats.DynamoValue
import com.engitano.dynamof.formats.ToDynamoValue

sealed abstract class UpdateExpression
sealed abstract class SetExpression
case class Pure(v: DynamoValue) extends SetExpression
case class Addition(v: DynamoValue) extends SetExpression
case class Subtraction(v: DynamoValue) extends SetExpression
case class IfNotExists(exp: SetExpression) extends SetExpression
case class ListAppend(vals: DynamoValue.L) extends SetExpression
case class ListPrepend(vals: DynamoValue.L) extends SetExpression
case class Set(f: Symbol, v: SetExpression) extends UpdateExpression
case class Add(f: Symbol, v: DynamoValue) extends UpdateExpression
case class Remove(s: Symbol) extends UpdateExpression
case class Delete(s: Symbol, v: DynamoValue) extends UpdateExpression
case class And(lhs: UpdateExpression, rhs: UpdateExpression)


trait UpdateSymbolOps {
    import shapeless._
    import shapeless.labelled._
    import com.engitano.dynamof.formats.syntax._
    type K <: Symbol
    type Repr <: HList
    protected val w: Witness.Aux[K]
    def :=[KV: ToDynamoValue](v: KV): UpdateExpression   = Set(w.value, Pure(v.toDynamo))
    def +=[KV: ToDynamoValue](v: KV): UpdateExpression   = Set(w.value, Addition(v.toDynamo))
    def -=[KV: ToDynamoValue](v: KV): UpdateExpression   = Set(w.value, Subtraction(v.toDynamo))
    def ++=[KV: ToDynamoValue](v: List[KV]): UpdateExpression   = Set(w.value, ListAppend(DynamoValue.L(v.map(_.toDynamo))))
    def ::=[KV: ToDynamoValue](v: List[KV]): UpdateExpression   = Set(w.value, ListPrepend(DynamoValue.L(v.map(_.toDynamo))))
    def +[KV: ToDynamoValue](v: KV): UpdateExpression   = Add(w.value, v.toDynamo)
    def del[KV: ToDynamoValue](v: KV): UpdateExpression = Delete(w.value, v.toDynamo)

}
