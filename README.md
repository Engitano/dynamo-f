# Dynamo F 

## Purely Functional, Typesafe, Non-Blocking DynamoDB Access

#### !Not production ready. Don't use. You have been warned!

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
Without needing 'Free' interpreters

#### Usage

```scala
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

object CrudSpec {
  case class User(id: NonEmptyString, name: NonEmptyString)
}

class CrudSpec extends WordSpec with Matchers {
  import syntax.all._
  import syntax.{between => btwn}
  import com.engitano.dynamof.formats.DynamoValue._

  val lowLevelClient = DynamoDbAsyncClient
    .builder()
    .endpointOverride(new URI("http://localhost:8000"))
    .region(Region.AP_SOUTHEAST_2)
    .build()

  val client = DynamoFClient[IO](lowLevelClient)

  "DynamoF" should {
    "CRUD items" in {
      val table        = Table[User]("users", 'id)
      val expectedUser = User(dyn"1", dyn"Fred")
      val create       = table.create(1, 1)
      val put          = table.put(expectedUser)
      val get          = table.get(dyn"1")
      val del          = table.delete(dyn"1")

      val program = for {
        _ <- client.createTable(create)
        _ <- client.putItem(put)
        g <- client.getItem(get)
        _ <- client.deleteItem(del)
        h <- client.getItem(get)
      } yield (g, h)

      program.unsafeRunSync shouldBe (Some(expectedUser), None)
    }
    "List items" in {
      val table         = Table[User]("users2", 'id, 'name)
      val expectedUser1 = User(dyn"1", dyn"Fred")
      val expectedUser2 = User(dyn"1", dyn"Joe")
      val putFred       = table.put(expectedUser1)
      val putJoe        = table.put(expectedUser2)
      val listUsers     = table.list(dyn"1")

      val program = for {
        _     <- client.createTable(table.create(1, 1))
        _     <- (client.putItem(putFred), client.putItem(putJoe)).tupled
        items <- client.listItems(listUsers)
      } yield items

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1, expectedUser2), None)
    }

    "Query items" in {
      val table         = Table[User]("users", 'id, 'name)
      val expectedUser1 = User(dyn"1", dyn"Fred", 25, 180)
      val expectedUser2 = User(dyn"1", dyn"Michael", 32, 152)
      val expectedUser3 = User(dyn"1", dyn"Nick", 19, 180)
      val expectedUser4 = User(dyn"1", dyn"Zoe", 30, 180)
      val putFred       = table.put(expectedUser1)
      val putFreddy     = table.put(expectedUser2)
      val putFreddo     = table.put(expectedUser3)
      val putJoe        = table.put(expectedUser4)
      val findFred      = table.query(dyn"1", beginsWith(dyn"Fre"), 'age > 20 and 'heightCms > 152, limit = Some(5), startAt = Some((dyn"1", dyn"Fre")))

      val program = client.useTable(table.create(1, 1)) {
        for {
          _     <- (client.putItem(putFred), client.putItem(putFreddy),client.putItem(putFreddo), client.putItem(putJoe)).tupled
          items <- client.queryItems(findFred)
        } yield items
      }

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1), None)
    }
}

```
