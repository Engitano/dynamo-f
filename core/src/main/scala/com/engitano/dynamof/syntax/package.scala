package com.engitano.dynamof
import shapeless.LabelledGeneric
import shapeless.ops.record.Keys
import shapeless.HList

package object syntax {

  trait AllSyntax extends TableSyntax with ToScalarAttrInstances with FilterSyntax with ToPredicateInstances with IsPrimaryKeySyntax with FieldNamesSyntax

  object table              extends TableSyntax
  object isPrimaryKey       extends IsPrimaryKeySyntax
  object filter             extends FilterSyntax
  object scalarAttributes   extends ToScalarAttrInstances
  object all                extends AllSyntax
}

trait FieldNames[T] {
  type Repr
}

object FieldNames {
  type Aux[T, R0] = FieldNames[T] { type Repr = R0 }
}

trait FieldNamesSyntax {
  implicit def apply[T, LGRepr <: HList, K <: HList](implicit lg: LabelledGeneric.Aux[T, LGRepr], keys: Keys.Aux[LGRepr, K]): FieldNames.Aux[T, K] =
    new FieldNames[T] {
      type Repr = K
    }
}
