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
  case class User(id: NonEmptyString, name: NonEmptyString, age: Int, heightCms: Int)
}

class CrudSpec extends WordSpec with Matchers {
  import testSyntax._

  val lowLevelClient = DynamoDbAsyncClient
    .builder()
    .endpointOverride(new URI("http://localhost:8000"))
    .region(Region.AP_SOUTHEAST_2)
    .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create("key","secret")))
    .build()

  val client = DynamoFClient[IO](lowLevelClient)

  "DynamoF" should {
    "CRUD items" in {
      val table        = Table[User]("users", 'id)
      val expectedUser = User(nes"1", nes"Fred", 25, 180)
      val create       = table.create(1, 1, Seq(), Seq())
      val put          = table.put(expectedUser)
      val get          = table.get(nes"1")
      val del          = table.delete(nes"1")

      val program = client.useTable(create) {
        for {
          _ <- client.putItem(put)
          g <- client.getItem(get)
          _ <- client.deleteItem(del)
          h <- client.getItem(get)
        } yield (g, h)
      }

      program.unsafeRunSync shouldBe (Some(expectedUser), None)
    }
    "List items" in {
      val table         = Table[User]("users", 'id, 'age)
      val expectedUser1 = User(nes"1", nes"Fred", 25, 180)
      val expectedUser2 = User(nes"1", nes"Joe", 30, 180)
      val putFred       = table.put(expectedUser1)
      val putJoe        = table.put(expectedUser2)
      val listUsers     = table.list(nes"1")

      val program = client.useTable(table.create(1, 1, Seq(), Seq())) {
        for {
          _     <- (client.putItem(putFred), client.putItem(putJoe)).tupled
          items <- client.listItems(listUsers)
        } yield items
      }

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1, expectedUser2), None)
    }

    "Query items" in {
      val table         = Table[User]("users", 'id, 'name)
      val expectedUser1 = User(nes"1", nes"Fred", 25, 180)
      val expectedUser2 = User(nes"1", nes"Michael", 32, 152)
      val expectedUser3 = User(nes"1", nes"Nick", 19, 180)
      val expectedUser4 = User(nes"1", nes"Zoe", 30, 180)
      val putFred       = table.put(expectedUser1)
      val putFreddy     = table.put(expectedUser2)
      val putFreddo     = table.put(expectedUser3)
      val putJoe        = table.put(expectedUser4)
      val findFred      = table.query(nes"1", beginsWith(nes"Fre"), 'age > 20 and 'heightCms > 152, limit = Some(5), startAt = Some((nes"1", nes"Fre")))

      val program = client.useTable(table.create(1, 1, Seq(), Seq())) {
        for {
          _     <- (client.putItem(putFred), client.putItem(putFreddy),client.putItem(putFreddo), client.putItem(putJoe)).tupled
          items <- client.queryItems(findFred)
        } yield items
      }

      program.unsafeRunSync() shouldBe QueryResponse(List(expectedUser1), None)
    }
  }
}
