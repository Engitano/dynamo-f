# Dynamo F 

## Purely Functional, Typesafe, Non-Blocking DynamoDB Access

### Strongly Typed.
Using the types in `com.engitano.dynamof.syntax.all._`, compliance to the the DynamoDB domain rules are checked by the compiler.
i.e
* No empty string fields.
* Can only query tables with a compound key.
* Query key expression applies only to range key.
* Query filter expression cannot include Key fields.

### Non Blocking
Uses Amazon SDK V2 under the hood with non-blocking goodness.

### BYO Effect Type.
Use any effect type that has a cats Async instance: cats.IO, Monix, ZIO

#### Usage

```scala
import com.engitano.dynamof.syntax.all._
import com.engitano.dynamof.formats._
import com.engitano.dynamof.formats.auto._
import cats.effect.IO
import cats.syntax.apply._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.engitano.dynamof.CrudSpec.User
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import java.net.URI
import software.amazon.awssdk.regions.Region
import cats.effect.Async
import cats.effect.Resource
import shapeless.HNil
import com.engitano.dynamof.syntax.beginsWith
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider

object CrudSpec {
  case class User(id: DynamoString, name: DynamoString, age: Int, heightCms: Int)
}

class CrudSpec extends WordSpec with Matchers {

  val lowLevelClient = DynamoDbAsyncClient
    .builder()
    .endpointOverride(new URI("http://localhost:8000"))
    .region(Region.AP_SOUTHEAST_2)
    .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create("key", "secret")))
    .build()

  val interpreter = AwsSdkInterpreter[IO](lowLevelClient)

  "DynamoF" should {
    "CRUD items" in {
      val table        = Table[User]("users", 'id)
      val expectedUser = User(dyn"1", dyn"Fred", 25, 180)
      val get          = table.get(dyn"1")

      val prog = for {
        _ <- table.create(1, 1, Seq(), Seq())
        g <- get
        _ <- table.put(expectedUser)
        h <- get
        _ <- table.delete(dyn"1")
        _ <- table.drop()
      } yield (g, h)

      prog.eval(interpreter).unsafeRunSync shouldBe (None, Some(expectedUser))
    }
    
    "List items" in {
      val table         = Table[User]("users", 'id, 'age)
      val expectedUser1 = User(dyn"1", dyn"Fred", 25, 180)
      val expectedUser2 = User(dyn"1", dyn"Joe", 30, 180)

      val prog = for {
        _     <- table.create(1, 1, Seq(), Seq())
        _     <- table.put(expectedUser1)
        _     <- table.put(expectedUser2)
        items <- table.list(dyn"1")
        _     <- table.drop()
      } yield items

      prog.eval(interpreter).unsafeRunSync() shouldBe QueryResponse(List(expectedUser1, expectedUser2), None)
    }

    "Query items" in {
      val table         = Table[User]("users", 'id, 'name)
      val expectedUser1 = User(dyn"1", dyn"Fred", 25, 180)
      val expectedUser2 = User(dyn"1", dyn"Michael", 32, 152)
      val expectedUser3 = User(dyn"1", dyn"Nick", 19, 180)
      val expectedUser4 = User(dyn"1", dyn"Zoe", 30, 180)

        val prog = for {
          _ <- table.create(1, 1, Seq(), Seq())
          _ <- table.put(expectedUser1)
          _ <- table.put(expectedUser2)
          _ <- table.put(expectedUser3)
          _ <- table.put(expectedUser4)
          items <- table.query(
            dyn"1",
            beginsWith(dyn"Fre"),
            'age > 20 and 'heightCms > 152,
            limit = Some(5),
            startAt = Some((dyn"1", dyn"Fre"))
          )
          _ <- table.drop()
        } yield items
      

      prog.eval(interpreter).unsafeRunSync() shouldBe QueryResponse(List(expectedUser1), None)
    }
  }
}


```
