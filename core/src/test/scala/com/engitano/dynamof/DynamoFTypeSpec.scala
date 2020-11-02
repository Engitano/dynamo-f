package com.engitano.dynamof

import shapeless.syntax.singleton._
import shapeless.HNil
import com.engitano.dynamof._
import com.engitano.dynamof.implicits._
import com.engitano.dynamof.formats.implicits._
import com.engitano.dynamof.DynamoFTypeSpec.MyDto
import org.scalatest.{Matchers, WordSpec}
import eu.timepit.refined.types.string.NonEmptyString
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

object DynamoFTypeSpec {
  case class MyDto(id: NonEmptyString, name: NonEmptyString, dob: Long)
}

class DynamoFTypeSpec extends WordSpec with Matchers {


  "The DynamoF Type System" should {
    "return a GetItemRequest when fetching by hash key" in {
      val t = Table[MyDto]("TestTable", 'id)
      t.get(dyn"3")
      t.set(dyn"3", 'name ->> dyn"fred" :: HNil)
      // req should matchPattern {
      //   case GetItemRequest("TestTable", M(m), _) if m == Map("id" -> S("3")) =>
      // }
    }

    "return a GetItemRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      t.get(dyn"3" -> 123456789L)
      // req should matchPattern {
      //   case GetItemRequest("TestTable", M(m), _) if m == Map("id" -> S("3"), "dob" -> N("123456789")) =>
      // }
    }

    "return a ListItemsRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      t.list(dyn"3")
      // req should matchPattern {
      //   case ListItemsRequest("TestTable", "id" -> S("3"), None, _) =>
      // }
    }

    "return a local secondary index instance" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req   = t.localSecondaryIndex("nameIndex", 'name)
      val ixDef = req.definition
      ixDef.key shouldBe CompositeKey(AttributeDefinition("id", ScalarAttributeType.S), AttributeDefinition("name", ScalarAttributeType.S))

      req.query(dyn"123", lt(dyn"b"), 'dob >= 100, Some(5), Some(dyn"123" -> dyn"b"))
      // queryRequest should matchPattern {
      //   case QueryRequest(
      //       "TestTable",
      //       ("id", S("123")),
      //       LessThan("name", S("b")),
      //       Some(5),
      //       Some(GreaterThanOrEquals("dob", N("100"))),
      //       Some(M(m)),
      //       Some("nameIndex"),
      //       _
      //       ) if m == Map("id" -> S("123"), "name" -> S("b")) =>
      // }
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
