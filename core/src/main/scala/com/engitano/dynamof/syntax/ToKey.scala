package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._

sealed trait ToKey[A] {
  def toKey(a: A): DynamoValue.M
}

trait ToKeySyntax {

  implicit def toHashKey[V: ToDynamoValue] = new ToKey[(SimpleKey, V)] {
    def toKey(a: (SimpleKey, V)): DynamoValue.M = DynamoValue.M(Map(a._1.attribute.name -> ToDynamoValue[V].to(a._2)))
  }
  implicit def toHashKeyOther[H: ToDynamoValue, R: ToDynamoValue] = new ToKey[(CompositeKey, (H, R))] {
    def toKey(a: (CompositeKey, (H, R))): DynamoValue.M =
      DynamoValue.M(
        Map(
          a._1.hashKey.name  -> ToDynamoValue[H].to(a._2._1),
          a._1.rangeKey.name -> ToDynamoValue[R].to(a._2._2)
        )
      )
  }
}
