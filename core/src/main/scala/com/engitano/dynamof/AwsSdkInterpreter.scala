package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.syntax.all._
import cats.instances.either._
import cats.instances.list._
import cats.~>
import cats.syntax.apply._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.effect.Async
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import scala.jdk.CollectionConverters._
import software.amazon.awssdk.services.dynamodb.model.{DeleteTableRequest => JDeleteTableRequest}
import software.amazon.awssdk.services.dynamodb.model.{DescribeTableRequest => JDescribeTableRequest}
import software.amazon.awssdk.services.dynamodb.model.TableDescription
import software.amazon.awssdk.services.dynamodb.model.ResourceNotFoundException
import cats.effect.Sync
import cats.effect.IO
import cats.effect.Concurrent
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException

object AsyncCF {
  def wrap[F[_]: Async, A](cf: => CompletableFuture[A]): F[A] = Async[F].async(cb =>
      cf.handle[Unit] { (a, t) =>
        (Option(a), Option(t)) match {
          case (Some(a), None) => cb(Right(a))
          case (None, Some(e:CompletionException)) => cb(Left(e.getCause()))
          case (None, Some(t)) => cb(Left(t))
          case _               => cb(Left(new Exception("Impossible CompletableFuture State")))
        }
      })
}

object AwsSdkInterpreter {
  def apply[F[_]](client: DynamoDbAsyncClient)(implicit F: Async[F]): DynamoOpA ~> F = new (DynamoOpA ~> F) {
    def apply[A](fa: DynamoOpA[A]): F[A] =
      fa match {
        case DescribeTableRequest(name) =>
          AsyncCF.wrap(client
            .describeTable(JDescribeTableRequest.builder().tableName(name).build()))
            .map(r => r.table().some)
            .handleErrorWith {
              case _: ResourceNotFoundException => Sync[F].pure(none[TableDescription])
              case t                            => Sync[F].raiseError(t)
            }
        case req: CreateTableRequest =>
          AsyncCF.wrap(client
            .createTable(JavaRequests.to(req)))
            .as(())
        case DeleteTableRequest(name) =>
          AsyncCF.wrap(client
            .deleteTable(JDeleteTableRequest.builder().tableName(name).build()))
            .as(())
        case req: GetItemRequest[_] =>
          AsyncCF.wrap(
            client
              .getItem(JavaRequests.to(req)))
              .flatMap { r =>
                if (r.item().isEmpty())
                  F.pure(None)
                else
                  req.fdv.from(DynamoValue.M.parse(r.item.asScala.toMap)).map(i => i.some).liftTo[F]
              }
        case req: PutItemRequest =>
          AsyncCF.wrap(client
            .putItem(JavaRequests.to(req)))
            .as(())
        case req: DeleteItemRequest =>
          AsyncCF.wrap(client
            .deleteItem(JavaRequests.to(req)))
            .as(())
        case req: ListItemsRequest[_] =>
          AsyncCF.wrap(client
            .query(JavaRequests.to(req)))
            .map(r => (r.items().asScala.map(v => DynamoValue.M.parse(v.asScala.toMap)), r.lastEvaluatedKey()))
            .flatMap(
              f =>
                f._1.toList
                  .traverse(p => req.fdv.from(p))
                  .map(r => QueryResponse(r, if (f._2.isEmpty()) None else Some(DynamoValue.M.parse(f._2.asScala.toMap))))
                  .liftTo[F]
            )
        case req: QueryRequest[_] =>
          AsyncCF.wrap(client
            .query(JavaRequests.to(req)))
            .map(r => (r.items().asScala.map(v => DynamoValue.M.parse(v.asScala.toMap)), r.lastEvaluatedKey()))
            .flatMap(
              f =>
                f._1.toList
                  .traverse(p => req.fdv.from(p))
                  .map(r => QueryResponse(r, if (f._2.isEmpty()) None else Some(DynamoValue.M.parse(f._2.asScala.toMap))))
                  .liftTo[F]
            )
      }
  }
}
