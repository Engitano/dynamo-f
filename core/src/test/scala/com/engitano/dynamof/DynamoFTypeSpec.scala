package com.engitano.dynamof

import com.engitano.dynamof.DynamoFTypeSpec.MyDto
import org.scalatest.{Matchers, WordSpec}
import eu.timepit.refined.types.string.NonEmptyString
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType
import com.engitano.dynamof.syntax.lt

object DynamoFTypeSpec {
  case class MyDto(id: NonEmptyString, name: NonEmptyString, dob: Long)
}

class DynamoFTypeSpec extends WordSpec with Matchers {

  import syntax.all._
  import formats.auto._
  import formats._
  import DynamoValue._

  "The DynamoF Type System" should {
    "return a GetItemRequest when fetching by hash key" in {
      val t = Table[MyDto]("TestTable", 'id)

      val req = t.get(nes"3")
      req shouldBe GetItemRequest("TestTable", M(Map("id" -> S("3"))))
    }

    "return a GetItemRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.get(nes"3" -> 123456789L)
      req shouldBe GetItemRequest("TestTable", M(Map("id" -> S("3"), "dob" -> N("123456789"))))
    }

    "return a ListItemsRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.list(nes"3")
      req shouldBe ListItemsRequest("TestTable", "id" -> S("3"), None)
    }

    "return a local secondary index instance" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req   = t.localSecondaryIndex("nameIndex", 'name)
      val ixDef = req.definition
      ixDef.key shouldBe CompositeKey(AttributeDefinition("id", ScalarAttributeType.S), AttributeDefinition("name", ScalarAttributeType.S))

      val queryRequest = req.query(nes"123", lt(nes"b"), 'dob >= 100, Some(5), Some(nes"123", nes"b"))
      queryRequest shouldBe QueryRequest(
          "TestTable",
          ("id", S("123")),
          LessThan("name", S("b")),
          Some(5),
          Some(GreaterThanOrEquals("dob", N("100"))),
          Some(M(Map("id" -> S("123"), "name" -> S("b")))),
          Some("nameIndex")
        )
    }

    "return a global secondary index instance" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req   = t.globalSecondaryIndex("nameIndex", 'name, 'dob)
      val ixDef = req.definition(1L, 1L)
      ixDef.name shouldBe "nameIndex"
      ixDef.table shouldBe "TestTable"
      ixDef.key shouldBe CompositeKey(AttributeDefinition("name", ScalarAttributeType.S), AttributeDefinition("dob", ScalarAttributeType.N))

    }
  }
}
