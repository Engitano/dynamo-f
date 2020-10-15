package com.engitano.dynamof.syntax

import shapeless.LabelledGeneric
import shapeless.ops.record.Keys
import shapeless.HList

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
