package com.engitano

import cats.~>
import cats.free.{ Free, FreeApplicative }
import cats.Applicative

package object dynamof {

    type DynamoOp[A] = Free[FreeApplicative[DynamoOpA, ?], A]

    def parallelInterpreter[F[_]](interpreter: DynamoOpA ~> F)(implicit A: Applicative[F]): FreeApplicative[DynamoOpA, ?] ~> F = new (FreeApplicative[DynamoOpA, ?] ~> F) {
        def apply[A](fa: FreeApplicative[DynamoOpA,A]): F[A] = fa.foldMap(interpreter)
    }


}