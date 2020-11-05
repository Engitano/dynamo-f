package com.engitano.dynamof.formats

import eu.timepit.refined.types.string.NonEmptyString


object auto extends AutoFormats 
object instances extends MarshallerInstances

object syntax {

  implicit class NonEmptyStringHelper(val sc: StringContext) extends AnyVal {
    def dyn(args: Any*): NonEmptyString = macro NonEmptyStringMacros.nesImpl
  }

  class ToDynamoSyntax[V](v: V, tdv: ToDynamoValue[V]) {
    def toDynamo = tdv.to(v)
  }

  implicit def toDynamoSyntax[V](v: V)(implicit tdv: ToDynamoValue[V]) = new ToDynamoSyntax[V](v, tdv)
}
