package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

trait ToScalarAttr[A] {
  type S <: ScalarAttributeType
  def to: S
}

object ToScalarAttr {
  type Aux[A, S0 <: ScalarAttributeType] = ToScalarAttr[A] { type S = S0 }
}

trait ToScalarAttrInstances {
  implicit def scalarAttrTypeForInt: ToScalarAttr.Aux[Int, ScalarAttributeType.N.type] = new ToScalarAttr[Int] {
    type S = ScalarAttributeType.N.type
    def to: S = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForLong: ToScalarAttr.Aux[Long, ScalarAttributeType.N.type] = new ToScalarAttr[Long] {
    type S = ScalarAttributeType.N.type
    def to: S = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForBool: ToScalarAttr.Aux[Boolean, ScalarAttributeType.B.type] = new ToScalarAttr[Boolean] {
    type S = ScalarAttributeType.B.type
    def to: S = ScalarAttributeType.B
  }
  implicit def scalarAttrTypeForNes: ToScalarAttr.Aux[formats.DynamoString, ScalarAttributeType.S.type] = new ToScalarAttr[formats.DynamoString] {
    type S = ScalarAttributeType.S.type
    def to: S = ScalarAttributeType.S
  }
}
