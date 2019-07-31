# Dynamo F - Purely Functional, Typesafe, Non-Blocking DynamoDB Access

### Nowhere near production ready. Don't use.

#### Usage

```scala
import com.engitano.dynamof.formats._
import com.engitano.dynamof.formats.auto._
import cats.effect.IO
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
    import com.engitano.dynamof.formats.DynamoValue._

    val lowLevelClient = DynamoDbAsyncClient
                            .builder()
                            .endpointOverride(new URI("http://localhost:8000"))
                            .region(Region.AP_SOUTHEAST_2).build()

    "DynamoF" should {
        "CRUD items" in {
            val expectedUser = User(nes"1", nes"Fred")
            val table = Table[User]("users", 'id)
            val create = table.create(1, 1)
            val put = table.put(expectedUser)
            val get = table.get(nes"1")
            val del = table.delete(nes"1")
            val client = DynamoFClient[IO](lowLevelClient)
            val program = for {
                _ <- client.createTable(create)
                _ <- client.putItem(put)
                g <- client.getItem(get)
                _ <- client.deleteItem(del)
                h <- client.getItem(get)
            } yield (g, h)
            
            program.unsafeRunSync shouldBe (Some(expectedUser), None)
        }
    }
}
```