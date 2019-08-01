/*
 * Copyright (c) 2019 Engitano
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.engitano.dynamof

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
      val expectedUser = User(nes"1", nes"Fred")
      val create       = table.create(1, 1)
      val put          = table.put(expectedUser)
      val get          = table.get(nes"1")
      val del          = table.delete(nes"1")

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
      val expectedUser1 = User(nes"1", nes"Fred")
      val expectedUser2 = User(nes"1", nes"Joe")
      val putFred       = table.put(expectedUser1)
      val putJoe        = table.put(expectedUser2)
      val listUsers     = table.list(nes"1")

      val program = for {
        _     <- client.createTable(table.create(1, 1))
        _     <- (client.putItem(putFred), client.putItem(putJoe)).tupled
        items <- client.listItems(listUsers)
      } yield items

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1, expectedUser2), None)
    }

    "Query items" in {
      import com.engitano.dynamof.syntax._
      val table         = Table[User]("users3", 'id, 'name)
      val expectedUser1 = User(nes"1", nes"Fred")
      val expectedUser2 = User(nes"1", nes"Joe")
      val putFred       = table.put(expectedUser1)
      val putJoe        = table.put(expectedUser2)
      val findFred      = table.query(nes"1", btwn(nes"Eddie", nes"Gary"))

      val program = for {
        _     <- client.createTable(table.create(1, 1))
        _     <- (client.putItem(putFred), client.putItem(putJoe)).tupled
        items <- client.queryItems(findFred)
      } yield items

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1), None)
    }
  }
}
