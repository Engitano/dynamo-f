package com.engitano.dynamof

import com.engitano.dynamof.DynamoFTypeSpec.MyDto
import org.scalatest.{Matchers, WordSpec}
import eu.timepit.refined.types.string.NonEmptyString

object DynamoFTypeSpec {
  case class MyDto(id: NonEmptyString, name: NonEmptyString, dob: Long)
}

class DynamoFTypeSpec extends WordSpec with Matchers {

  import ToKey._
  import formats.auto._
  import formats._
  import DynamoValue._

  "The DynamoF Type System" should {
    "return a GetItemRequest when fetching by hash key" in {
      val t = Table[MyDto]("TestTable", 'id)

      val req = t.get(nes"3")
      req shouldBe GetItemRequest("TestTable", M(Map("id" -> S("3"))))
    }

    "Return a GetItemRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.get(nes"3" -> 123456789L)
      req shouldBe GetItemRequest("TestTable", M(Map("id" -> S("3"), "dob" -> N("123456789"))))
    }
    
    "Return a QueryRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.query(nes"3")
      req shouldBe QueryRequest("TestTable", M(Map("id" -> S("3"))))
    }
  }
}
