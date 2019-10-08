package com.engitano.dynamof.formats

import org.scalatest.WordSpec
import org.scalatest.Matchers

import cats.implicits._
import eu.timepit.refined.collection.NonEmpty
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import java.time.LocalDate
import java.time.LocalDateTime
import org.scalacheck.ScalacheckShapeless._
import com.engitano.dynamof.formats.AutoFormatsSpec._

object AutoFormatsSpec {
    case class TestStruct(id: DynamoString, age: Int)
    case class TestOptionStruct(id: DynamoString, age: Option[Int])
    case class TestOptionList(id: DynamoString, age: Seq[Int])
}

class AutoFormatsSpec extends WordSpec with Matchers with Checkers {
    import DynamoValue._
    "AutoDerivation" should {
        import auto._
        type F[A] = Either[Throwable, A]
        "correctly map a struct" in {
            val to = ToDynamoValue[TestStruct]
            val from = FromDynamoValue[F, TestStruct]
            to.to(TestStruct(dyn"123", 21)) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestStruct(dyn"321", 12))
        }
        "correctly map a struct to a map" in {
            val to = ToDynamoMap[TestStruct]
            val from = FromDynamoValue[F, TestStruct]
            to.to(TestStruct(dyn"123", 21)) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestStruct(dyn"321", 12))
        }
        "correctly map a struct with optional values when dynamo values are supplied" in {
            val to = ToDynamoValue[TestOptionStruct]
            val from = FromDynamoValue[F, TestOptionStruct]
            to.to(TestOptionStruct(dyn"123", Some(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestOptionStruct(dyn"321", Some(12)))
        }
        "correctly map a struct with optional values when dynamo values are not supplied" in {
            val from = FromDynamoValue[F, TestOptionStruct]
            from.from(M(Map("id" -> S("321")))) shouldBe Right(TestOptionStruct(dyn"321", None))
        }

        "correctly map a struct with optional values when dynamo values are Null" in {
            val to = ToDynamoValue[TestOptionStruct]
            val from = FromDynamoValue[F, TestOptionStruct]
            to.to(TestOptionStruct(dyn"123", Some(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> Null))) shouldBe Right(TestOptionStruct(dyn"321", None))
        }

        "correctly map a struct with Seq values when dynamo values are Null" in {
            val to = ToDynamoValue[TestOptionList]
            val from = FromDynamoValue[F, TestOptionList]
            to.to(TestOptionList(dyn"123", List(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> L(List(N("21")))))
            from.from(M(Map("id" -> S("321"), "age" -> L(List(N("21")))))) shouldBe Right(TestOptionList(dyn"321", List(21)))
        }
        "map to and from scala type" when {
            "type is int" in {
                val to = ToDynamoValue[Int]
                val from = FromDynamoValue[F, Int]
                check((i: Int) => from.from(to.to(i)) == Right(i))
            }

            "type is float" in {
                val to = ToDynamoValue[Float]
                val from = FromDynamoValue[F, Float]
                check((i: Float) => from.from(to.to(i)) == Right(i))
            }

            "type is double" in {
                val to = ToDynamoValue[Double]
                val from = FromDynamoValue[F, Double]
                check((i: Double) => from.from(to.to(i)) == Right(i))
            }

            "type is non emptyString" in {
                val to = ToDynamoValue[DynamoString]
                val from = FromDynamoValue[F, String]
                check { (i: String) => 
                    i.length() > 0 ==> (from.from(to.to(eu.timepit.refined.refineV[NonEmpty].unsafeFrom(i))) == Right(i))
                }
            }
        }
    }
}