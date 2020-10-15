package com.engitano.dynamof.formats

import eu.timepit.refined.types.string.NonEmptyString

object implicits extends MarshallerInstances with AutoFormats {

  implicit class NonEmptyStringHelper(val sc: StringContext) extends AnyVal {
    def dyn(args: Any*): NonEmptyString = macro NonEmptyStringMacros.nesImpl
  }

  class ToDynamoSyntax[V](v: V)(implicit tdv: ToDynamoValue[V]) {
    def toDynamo = tdv.to(v)
  }

  implicit def toDynamoSyntax[V: ToDynamoValue](v: V) = new ToDynamoSyntax[V](v)
}
