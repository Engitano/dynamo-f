package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.syntax._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.effect.Async
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import scala.jdk.CollectionConverters._
import software.amazon.awssdk.services.dynamodb.model.DeleteTableRequest

trait DynamoFClient[F[_]] {
  type Serializer[A]   = ToDynamoValue[A]
  type Deserializer[A] = FromDynamoValue[F, A]

  def createTable(req: CreateTableRequest): F[Unit]
  def putItem(req: PutItemRequest): F[Unit]
  def getItem[A: Deserializer](req: GetItemRequest[A]): F[Option[A]]
  def deleteItem(req: DeleteItemRequest): F[Unit]
  def listItems[A: Deserializer](req: ListItemsRequest[A]): F[QueryResponse[A]]
  def queryItems[A](req: QueryRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[QueryResponse[A]]
  def deleteTable(name: String): F[Unit]
}

object DynamoFClient {
  import syntax.completableFutures._
  def apply[F[_]](client: DynamoDbAsyncClient)(implicit F: Async[F]): DynamoFClient[F] = new DynamoFClient[F] {

    def createTable(req: CreateTableRequest): F[Unit] =
      client
        .createTable(JavaRequests.to(req))
        .lift[F]
        .as(())

    def deleteTable(name: String): F[Unit] =
      client
        .deleteTable(DeleteTableRequest.builder().tableName(name).build())
        .lift[F]
        .as(())

    def getItem[A](req: GetItemRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[Option[A]] =
      client
        .getItem(JavaRequests.to(req))
        .lift[F]
        .flatMap { r =>
          if (r.item().isEmpty())
            F.pure(None)
          else
            fdv.from(DynamoValue.M.parse(r.item.asScala.toMap)).map(i => Some(i))
        }

    def putItem(req: PutItemRequest): F[Unit] =
      client
        .putItem(JavaRequests.to(req))
        .lift[F]
        .as(())

    def deleteItem(req: DeleteItemRequest): F[Unit] =
      client
        .deleteItem(JavaRequests.to(req))
        .lift[F]
        .as(())

    def listItems[A](req: ListItemsRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[QueryResponse[A]] =
      client
        .query(JavaRequests.to(req))
        .lift[F]
        .map(r => (r.items().asScala.map(v => DynamoValue.M.parse(v.asScala.toMap)), r.lastEvaluatedKey()))
        .flatMap(
          f =>
            f._1.toList
              .traverse(p => fdv.from(p))
              .map(r => QueryResponse(r, if (f._2.isEmpty()) None else Some(DynamoValue.M.parse(f._2.asScala.toMap))))
        )

    def queryItems[A](req: QueryRequest[A])(implicit fdv: FromDynamoValue[F, A]): F[QueryResponse[A]] =
      client
        .query(JavaRequests.to(req))
        .lift[F]
        .map(r => (r.items().asScala.map(v => DynamoValue.M.parse(v.asScala.toMap)), r.lastEvaluatedKey()))
        .flatMap(
          f =>
            f._1.toList
              .traverse(p => fdv.from(p))
              .map(r => QueryResponse(r, if (f._2.isEmpty()) None else Some(DynamoValue.M.parse(f._2.asScala.toMap))))
        )
  }
}
