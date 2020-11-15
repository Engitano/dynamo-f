package com.engitano.dynamof.formats


// import cats.implicits._
import eu.timepit.refined.collection.NonEmpty
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import java.time.OffsetDateTime
// import org.scalacheck.ScalacheckShapeless._
import com.engitano.dynamof.formats.auto._
import com.engitano.dynamof.formats.syntax._
import com.engitano.dynamof.formats.AutoFormatsSpec._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import eu.timepit.refined.types.string.NonEmptyString

object AutoFormatsSpec {
    case class TestStruct(id: DynamoString, age: Long)
    case class TestOptionStruct(id: DynamoString, age: Option[Int])
    case class TestOptionList(id: DynamoString, age: List[Int])
    sealed trait Animal
    case object Dog extends Animal
    case object Cat extends Animal
    case class Pet(name: DynamoString) extends Animal
    case class Zoo(animals: Set[Animal])
    case class People(names: Set[NonEmptyString])
}

class AutoFormatsSpec extends WordSpec with Matchers with Checkers {
    import DynamoValue._

    implicit def arbitraryOffsetDateTime = Arbitrary(Gen.oneOf(Seq(OffsetDateTime.now())))
    

    "AutoDerivation" should {
        "correctly map a struct" in {
            val to = ToDynamoValue[TestStruct]
            val from = FromDynamoValue[TestStruct]
            to.to(TestStruct(dyn"123", 21)) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map(s"id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestStruct(dyn"321", 12))
        }
        "map a string set to a dynamo SS value" in {
            val to = ToDynamoValue[People]
            val from = FromDynamoValue[People]
            val original = People(Set(dyn"John", dyn"Mark"))
            val serializedPeople = to.to(original)
            serializedPeople shouldBe DynamoValue.M(Map("names" -> DynamoValue.SS(Set(DynamoValue.S("John"), DynamoValue.S("Mark")))))
            val deserialized = from.from(serializedPeople)
            deserialized shouldBe Right(original)
        }
        "correctly map sum types" in {
            val to = ToDynamoValue[Animal]
            val from = FromDynamoValue[Animal]
            val serializedDog = DynamoValue.M(Map("Dog" -> DynamoValue.M(Map())))
            val serializedPet = DynamoValue.M(Map("Pet" -> DynamoValue.M(Map("name" -> DynamoValue.S("Fido")))))
            to.to(Dog) shouldBe serializedDog
            to.to(Pet(dyn"Fido")) shouldBe serializedPet
            from.from(serializedDog) shouldBe Right(Dog)
            from.from(serializedPet) shouldBe Right(Pet(dyn"Fido"))
        }

        "correctly map a product with sum types" in {
            val to = ToDynamoValue[Zoo]
            val from = FromDynamoValue[Zoo]
            val zoo = Zoo(Set(Dog, Pet(dyn"Fido"), Cat))
            val serialized = to.to(zoo)
            val expected = M(Map("animals" -> L(List(M(Map("Dog" -> M(Map()))), M(Map("Pet" -> M(Map("name" -> S("Fido"))))), M(Map("Cat" -> M(Map())))))))
            serialized shouldBe expected
            from.from(serialized) shouldBe Right(zoo)

        }
        "correctly map a struct to a map" in {
            val to = ToDynamoMap[TestStruct]
            val from = FromDynamoValue[TestStruct]
            to.to(TestStruct(dyn"123", 21)) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestStruct(dyn"321", 12))
        }
        "correctly map a struct with optional values when dynamo values are supplied" in {
            val to = ToDynamoValue[TestOptionStruct]
            val from = FromDynamoValue[TestOptionStruct]
            to.to(TestOptionStruct(dyn"123", Some(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> N("12")))) shouldBe Right(TestOptionStruct(dyn"321", Some(12)))
        }
        "correctly map a struct with optional values when dynamo values are not supplied" in {
            val from = FromDynamoValue[TestOptionStruct]
            from.from(M(Map("id" -> S("321")))) shouldBe Right(TestOptionStruct(dyn"321", None))
        }

        "correctly map a struct with optional values when dynamo values are Null" in {
            val to = ToDynamoValue[TestOptionStruct]
            val from = FromDynamoValue[TestOptionStruct]
            to.to(TestOptionStruct(dyn"123", Some(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> N("21")))
            from.from(M(Map("id" -> S("321"), "age" -> Null))) shouldBe Right(TestOptionStruct(dyn"321", None))
        }

        "correctly map a struct with Seq values when dynamo values are Null" in {
            val to = ToDynamoValue[TestOptionList]
            val from = FromDynamoValue[TestOptionList]
            to.to(TestOptionList(dyn"123", List(21))) shouldBe DynamoValue.M(Map("id" -> S("123"), "age" -> L(List(N("21")))))
            from.from(M(Map("id" -> S("321"), "age" -> L(List(N("21")))))) shouldBe Right(TestOptionList(dyn"321", List(21)))
        }
        "map to and from scala type" when {
            "type is int" in {
                val to = ToDynamoValue[Int]
                val from = FromDynamoValue[Int]
                check((i: Int) => from.from(to.to(i)) == Right(i))
            }

            "type is float" in {
                val to = ToDynamoValue[Float]
                val from = FromDynamoValue[Float]
                check((i: Float) => from.from(to.to(i)) == Right(i))
            }

            "type is double" in {
                val to = ToDynamoValue[Double]
                val from = FromDynamoValue[Double]
                check((i: Double) => from.from(to.to(i)) == Right(i))
            }

            "type is OffsetDateTime" in {
                val to = ToDynamoValue[OffsetDateTime]
                val from = FromDynamoValue[OffsetDateTime]
                check((i: OffsetDateTime) => from.from(to.to(i)) == Right(i))
            }

            "type is non emptyString" in {
                val to = ToDynamoValue[DynamoString]
                val from = FromDynamoValue[String]
                check { (i: String) => 
                    i.length() > 0 ==> (from.from(to.to(eu.timepit.refined.refineV[NonEmpty].unsafeFrom(i))) == Right(i))
                }
            }
        }
    }
}