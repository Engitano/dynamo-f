package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

trait ToScalarAttr[A] {
  def to: ScalarAttributeType
}

trait ToScalarAttrInstances {
  implicit def scalarAttrTypeForInt = new ToScalarAttr[Int] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForLong = new ToScalarAttr[Long] {
    def to = ScalarAttributeType.N
  }
  implicit def scalarAttrTypeForBool = new ToScalarAttr[Boolean] {
    def to = ScalarAttributeType.B
  }
  implicit def scalarAttrTypeForNes = new ToScalarAttr[formats.NonEmptyString] {
    def to = ScalarAttributeType.S
  }
}
