package com.engitano.dynamof

import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import eu.timepit.refined.types.string.NonEmptyString

package object formats {

  type DynamoDocument = java.util.Map[String, AttributeValue]

  val DynamoString = NonEmptyString

  type DynamoString = NonEmptyString

}
