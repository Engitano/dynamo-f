package com.engitano.dynamof

import shapeless.syntax.singleton._
import shapeless.HNil
import com.engitano.dynamof._
import com.engitano.dynamof.implicits._
import com.engitano.dynamof.formats.auto._
import com.engitano.dynamof.formats.syntax._
import com.engitano.dynamof.DynamoFTypeSpec.MyDto
import org.scalatest.{Matchers, WordSpec}
import eu.timepit.refined.types.string.NonEmptyString
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType
import com.engitano.dynamof.formats.DynamoValue
import com.engitano.dynamof.formats.DynamoUnmarshallException

object DynamoFTypeSpec {
  case class MyDto(id: NonEmptyString, name: NonEmptyString, dob: Long)
}

class DynamoFTypeSpec extends WordSpec with Matchers {

  import DynamoValue._


  "The DynamoF Type System" should {
    "return a GetItemRequest when fetching by hash key" in {
      val t = Table[MyDto]("TestTable", 'id)
      val req = t.getOp(dyn"3")
      req should matchPattern {
        case GetItemRequest("TestTable", M(m), _) if m == Map("id" -> S("3")) =>
      }
    }

    "return a GetItemRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.getOp(dyn"3" -> 123456789L)
      req should matchPattern {
        case GetItemRequest("TestTable", M(m), _) if m == Map("id" -> S("3"), "dob" -> N("123456789")) =>
      }
    }

    "return a ListItemsRequest when fetching by hash and range key" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req = t.listOp(dyn"3")
      req should matchPattern {
        case ListItemsRequest("TestTable", "id" -> S("3"), None, _, _) =>
      }
    }

    "return a local secondary index instance" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req   = t.localSecondaryIndex("nameIndex", 'name)
      val ixDef = req.definition
      ixDef.key shouldBe CompositeKey(AttributeDefinition("id", ScalarAttributeType.S), AttributeDefinition("name", ScalarAttributeType.S))

      val queryRequest = req.queryOp(dyn"123", lt(dyn"b"), 'dob >= 100, Some(5), Some(dyn"123" -> dyn"b"))
      queryRequest should matchPattern {
        case QueryRequest(
            "TestTable",
            ("id", DynamoValue.S("123")),
            LessThan("name", DynamoValue.S("b")),
            Some(5),
            Some(GreaterThanOrEquals("dob", DynamoValue.N("100"))),
            Some(DynamoValue.M(m)),
            Some("nameIndex"),
            _,
            true
            ) if m == Map("id" -> DynamoValue.S("123"), "name" -> DynamoValue.S("b")) =>
      }
    }

    "return a global secondary index instance" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val req   = t.globalSecondaryIndex("nameIndex", 'name, 'dob)
      val ixDef = req.definition(1L, 1L)
      ixDef.name shouldBe "nameIndex"
      ixDef.table shouldBe "TestTable"
      ixDef.key shouldBe CompositeKey(AttributeDefinition("name", ScalarAttributeType.S), AttributeDefinition("dob", ScalarAttributeType.N))

    }

     "have a key that parses a valid dynamo map" in {
      val t = Table[MyDto]("TestTable", 'id, 'dob)

      val res: Either[DynamoUnmarshallException, (NonEmptyString, Long)] = t.parseKey(DynamoValue.M(Map("id" -> DynamoValue.S("123"), "dob" -> DynamoValue.N("312855060"))))
      res should matchPattern {
        case Right(v -> 312855060L) if v == dyn"123" => 
      }
    }
  }
}
