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

import com.engitano.dynamof._
import com.engitano.dynamof.implicits._
import com.engitano.dynamof.formats.DynamoString
import com.engitano.dynamof.formats.syntax._
import com.engitano.dynamof.formats.auto._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.engitano.dynamof.CrudSpec.{User, Car}
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import java.net.URI
import software.amazon.awssdk.regions.Region
import shapeless.syntax.singleton._
import shapeless.HNil
import cats.effect.Async
import cats.syntax.apply._
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import cats.effect.IO
import java.util.concurrent.Executors

object CrudSpec {
  case class User(id: DynamoString, name: DynamoString, age: Int, heightCms: Int)
  case class Car(id: Int, make: DynamoString)
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
        _ <- table.increment(dyn"1", 'age, 5)
        h <- get
        _ <- table.delete(dyn"1")
        _ <- table.drop()
      } yield (g, h)

      prog.eval(interpreter).unsafeRunSync shouldBe (None -> Some(expectedUser.copy(age = 30)))
    }
    "List items" in {
      val table = Table[User]("users", 'id, 'age)
      val ix    = table.localSecondaryIndex("usersByName", 'name)
      val fred  = User(dyn"1", dyn"Fred", 25, 180)
      val joe   = User(dyn"1", dyn"Joe", 30, 180)

      val prog = for {
        _   <- table.create(1, 1, Seq(ix.definition), Seq())
        _   <- table.put(fred)
        _   <- table.put(joe)
        joe <- ix.query(dyn"1", gt(dyn"J"))
        _   <- table.drop()
      } yield joe.results.headOption

      prog.eval(interpreter).unsafeRunSync() shouldBe Some(joe)
    }

    "Update items" in {
      val table = Table[User]("users", 'id)
      val joe   = User(dyn"1", dyn"Joe", 30, 180)

      val getJoe = table.get(dyn"1")

      val prog = for {
        _      <- table.create(1, 1, Seq(), Seq())
        _      <- table.put(joe)
        joe    <- getJoe
        _      <- table.set(dyn"1", 'age ->> 31 :: 'name ->> dyn"Fred" :: HNil)
        oldJoe <- getJoe
        _      <- table.drop()
      } yield (joe, oldJoe)

      val (jo, fred) = prog.eval(interpreter).unsafeRunSync()
      jo shouldBe Some(joe)
      fred shouldBe Some(joe.copy(age = 31, name = dyn"Fred"))
    }

    "Update items with compound key" in {
      val table = Table[User]("users", 'id, 'name)
      val joe   = User(dyn"1", dyn"Joe", 30, 180)

      val getJoe = table.get((dyn"1", dyn"Joe"))

      val prog = for {
        _      <- table.create(1, 1, Seq(), Seq())
        _      <- table.put(joe)
        joe    <- getJoe
        _      <- table.set((dyn"1", dyn"Joe"), 'age ->> 31 :: HNil)
        oldJoe <- getJoe
        _      <- table.drop()
      } yield (joe, oldJoe)

      val (joe1, joe2) = prog.eval(interpreter).unsafeRunSync()
      joe1 shouldBe Some(joe)
      joe2 shouldBe Some(joe.copy(age = 31))
    }

    "Transact Insert items" in {
      val cars           = Table[Car]("cars", 'id)
      val users          = Table[User]("users", 'id, 'age)
      val originalPeople = (1 to 5).map(i => User(dyn"1", dyn"Joe", i, 180))
      val originalCars   = (1 to 5).map(i => Car(i, dyn"BMW"))
      val putPeople      = originalPeople.map(users.putOp)
      val putCars        = originalCars.map(cars.putOp)

      val prog = for {
        _    <- (users.createP(1, 1, Seq(), Seq()), cars.createP(1, 1, Seq(), Seq())).tupled.seq
        _    <- Transactionally.write(putPeople.head, (putPeople.tail ++ putCars): _*)
        joes <- users.list(dyn"1")
        bmws <- cars.get(1)
        _    <- (users.dropP(), cars.dropP()).tupled.seq
      } yield (joes, bmws)

      val (allTheJoes, beemerOne) = prog.eval(interpreter).unsafeRunSync()
      allTheJoes.results shouldBe originalPeople
      beemerOne shouldBe originalCars.headOption
    }

    "Query items" in {
      val table         = Table[User]("users", 'id, 'name)
      val expectedUser1 = User(dyn"1", dyn"Fred", 25, 178)
      val expectedUser2 = User(dyn"1", dyn"Fredderick", 32, 152)
      val expectedUser3 = User(dyn"1", dyn"Nick", 19, 181)
      val expectedUser4 = User(dyn"1", dyn"Zoe", 30, 183)

      val prog = for {
        _ <- table.create(1, 1, Seq(), Seq())
        _ <- (table.putP(expectedUser1), table.putP(expectedUser2), table.putP(expectedUser3), table.putP(expectedUser4)).tupled.seq
        items <- table.query(
          dyn"1",
          beginsWith(dyn"Fre"),
          'age > 20 and 'heightCms > 152,
          limit = Some(5),
          startAt = Some((dyn"1", dyn"Fre"))
        )
        _ <- table.drop()
      } yield items

      //evalP enforces applicative operations are evaluated concurrently.
      //evalP requires a Parallel[F] to ensure F has the required Applicative[?]
      //Parallel[F] for IO requires a contextShit
      implicit val cs = IO.contextShift(scala.concurrent.ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4)))
      prog.evalP(interpreter).unsafeRunSync() shouldBe QueryResponse(List(expectedUser1), None)
    }
  }
}
