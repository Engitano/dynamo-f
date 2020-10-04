package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.syntax.all._
import cats.instances.either._
import cats.instances.list._
import cats.~>
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

object AwsSdkInterpreter {
  def apply[F[_]](client: DynamoDbAsyncClient)(implicit F: Async[F]): DynamoOpA ~> F = new (DynamoOpA ~> F) {
    def apply[A](fa: DynamoOpA[A]): F[A] =
      fa match {
        case DescribeTableRequest(name) =>
          client
            .describeTable(JDescribeTableRequest.builder().tableName(name).build())
            .lift[F]
            .map(r => r.table().some)
            .handleErrorWith {
              case _: ResourceNotFoundException => Sync[F].pure(none[TableDescription])
              case t                            => Sync[F].raiseError(t)
            }
        case req: CreateTableRequest =>
          client
            .createTable(JavaRequests.to(req))
            .lift[F]
            .as(())
        case DeleteTableRequest(name) =>
          client
            .deleteTable(JDeleteTableRequest.builder().tableName(name).build())
            .lift[F]
            .as(())
        case req: GetItemRequest[_] =>
          client
            .getItem(JavaRequests.to(req))
            .lift[F]
            .flatMap { r =>
              if (r.item().isEmpty())
                F.pure(None)
              else
                req.fdv.from(DynamoValue.M.parse(r.item.asScala.toMap)).map(i => i.some).liftTo[F]
            }
        case req: PutItemRequest =>
          client
            .putItem(JavaRequests.to(req))
            .lift[F]
            .as(())
        case req: DeleteItemRequest =>
          client
            .deleteItem(JavaRequests.to(req))
            .lift[F]
            .as(())
        case req: ListItemsRequest[_] =>
          client
            .query(JavaRequests.to(req))
            .lift[F]
            .map(r => (r.items().asScala.map(v => DynamoValue.M.parse(v.asScala.toMap)), r.lastEvaluatedKey()))
            .flatMap(
              f =>
                f._1.toList
                  .traverse(p => req.fdv.from(p))
                  .map(r => QueryResponse(r, if (f._2.isEmpty()) None else Some(DynamoValue.M.parse(f._2.asScala.toMap))))
                  .liftTo[F]
            )
        case req: QueryRequest[_] =>
          client
            .query(JavaRequests.to(req))
            .lift[F]
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
