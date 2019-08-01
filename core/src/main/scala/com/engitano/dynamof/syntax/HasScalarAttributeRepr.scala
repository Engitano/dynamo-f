package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

trait HasScalarAttributeRepr[A] {
  def to: ScalarAttributeType
}

trait HasScalarAttributeReprInstances {
  implicit def scalarAttrTypeForInt = new HasScalarAttributeRepr[Int] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForLong = new HasScalarAttributeRepr[Long] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForBool = new HasScalarAttributeRepr[Boolean] {
    def to = ScalarAttributeType.B
  }
  implicit def scalarAttrTypeForNes = new HasScalarAttributeRepr[formats.NonEmptyString] {
    def to = ScalarAttributeType.S
  }
}
