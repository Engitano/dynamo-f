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

import com.engitano.dynamof.syntax._
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
        _ <- table.createIfNotExists(1, 1, Seq(), Seq())
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
      val ix = table.localSecondaryIndex("usersByName", 'name)
      val fred = User(dyn"1", dyn"Fred", 25, 180)
      val joe = User(dyn"1", dyn"Joe", 30, 180)

      val prog = for {
        _     <- table.createIfNotExists(1, 1, Seq(ix.definition), Seq())
        _     <- table.put(fred)
        _     <- table.put(joe)
        joe   <- ix.query(dyn"1", gt(dyn"J"))
        _     <- table.drop()
      } yield joe.results.headOption

      prog.eval(interpreter).unsafeRunSync() shouldBe Some(joe)
    }

    "Query items" in {
      val table         = Table[User]("users", 'id, 'name)
      val expectedUser1 = User(dyn"1", dyn"Fred", 25, 180)
      val expectedUser2 = User(dyn"1", dyn"Michael", 32, 152)
      val expectedUser3 = User(dyn"1", dyn"Nick", 19, 180)
      val expectedUser4 = User(dyn"1", dyn"Zoe", 30, 180)

        val prog = for {
          _ <- table.createIfNotExists(1, 1, Seq(), Seq())
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
