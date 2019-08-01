package com.engitano.dynamof.syntax

import cats.effect.Async
import java.util.concurrent.CompletableFuture

trait CompletableFutureSyntax {

  implicit def toOps[A](cf: => CompletableFuture[A]): CompletableFutureOps[A] = new CompletableFutureOps[A](cf)

  class CompletableFutureOps[A](cf: => CompletableFuture[A]) {
    def lift[F[_]](implicit F: Async[F]): F[A] = F.async { cb =>
      cf.handle[Unit] { (a, t) =>
        (Option(a), Option(t)) match {
          case (Some(a), None) => cb(Right(a))
          case (None, Some(t)) => cb(Left(t))
          case _               => cb(Left(new Exception("Impossible CompletableFuture State")))
        }
      }
    }
  }
}
