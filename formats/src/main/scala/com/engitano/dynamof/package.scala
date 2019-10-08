package com.engitano.dynamof

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.macros.RefineMacro
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.language.experimental.macros

package object formats {
import eu.timepit.refined.types.string.NonEmptyString
import cats.ApplicativeError

    type DynamoDocument = java.util.Map[String, AttributeValue]

    val DynamoString = NonEmptyString

    type DynamoString = NonEmptyString

    implicit class NonEmptyStringHelper(val sc: StringContext) extends AnyVal {
        def dyn(args: Any*): NonEmptyString = macro formats.NonEmptyStringMacros.nesImpl
      }

      object syntax {

        class ToDynamoSyntax[V](v: V)(implicit tdv: ToDynamoValue[V]) {
          def toDynamo = tdv.to(v)
        }

        implicit def toDynamoSyntax[V: ToDynamoValue](v: V) = new ToDynamoSyntax[V](v)
      }
}