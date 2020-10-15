package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import cats.~>
import cats.Monad
import cats.Parallel

trait DynamoOpsSyntax {
    implicit def toDynamoOpSyntax[A](op: DynamoOp[A]) = new DynamoOpSyntax(op)
}

class DynamoOpSyntax[A](op: DynamoOp[A]) {
    def eval[F[_]: Monad](interpreter: DynamoOpA ~> F): F[A] = 
        op.foldMap(parallelInterpreter(interpreter))
    def evalP[F[_]: Monad: Parallel](interpreter: DynamoOpA ~> F)(implicit P: Parallel[F]) = 
        op.foldMap(parallelInterpreter(interpreter.andThen(P.parallel))(P.applicative).andThen(P.sequential))
}