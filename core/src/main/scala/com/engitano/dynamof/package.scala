package com.engitano

import cats.~>
import cats.free.Free
import cats.Monad

package object dynamof {
    type DynamoOp[A] = Free[DynamoOpA, A]

    implicit def toDynamoOpSyntax[A](op: DynamoOp[A]) = new DynamoOpSyntax(op)

    class DynamoOpSyntax[A](op: DynamoOp[A]) {
        def eval[F[_]: Monad](interpreter: DynamoOpA ~> F): F[A] = op.foldMap(interpreter)
    }
}