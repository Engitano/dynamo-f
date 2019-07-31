package com.engitano.dynamof

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.macros.RefineMacro
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.language.experimental.macros

package object formats {

    type DynamoDocument = java.util.Map[String, AttributeValue]

    type NonEmptyString = String Refined NonEmpty

    object NonEmptyString {
      def apply(t: String)(implicit rt: RefType[Refined], v: Validate[String, NonEmpty]): NonEmptyString =
        macro RefineMacro.impl[Refined, String, NonEmpty]

      def unapply(x: NonEmptyString): Some[String] = Some(x)
      
      def unsafeFromString(s: String): NonEmptyString = refineV[NonEmpty](s).getOrElse(throw new Exception("Non empty string expected. Empty string found"))

      def fromString(s: String): Either[String, NonEmptyString] = refineV[NonEmpty](s)
    }

    implicit class NonEmptyStringHelper(val sc: StringContext) extends AnyVal {
        def nes(args: Any*): NonEmptyString = macro formats.NonEmptyStringMacros.nesImpl
      }
}