package com.engitano

import cats.~>
import cats.free.{ Free, FreeApplicative }
import cats.Monad
import cats.instances.int
import com.engitano.dynamof.DynamoOpA
import cats.Applicative
import cats.CommutativeApplicative
import cats.Parallel
import cats.NonEmptyParallel

package object dynamof {

    object ExecutionStrategy {
        def unit = new ExecutionStrategy[Unit] {
            def par: FreeApplicative[DynamoOpA,Unit] = FreeApplicative.pure(())
            def seq: DynamoOp[Unit] = Free.pure(())
        }
    }
    trait ExecutionStrategy[A] {
        def seq: DynamoOp[A]
        def par: FreeApplicative[DynamoOpA, A]
    }


    type DynamoOp[A] = Free[FreeApplicative[DynamoOpA, ?], A]

    implicit def toDynamoOpSyntax[A](op: DynamoOp[A]) = new DynamoOpSyntax(op)
    implicit def toFreeApplicativeSyntax[A](op: FreeApplicative[DynamoOpA, A]) = new FreeApplicativeSyntax(op)

    def parallelInterpreter[F[_]](interpreter: DynamoOpA ~> F)(implicit A: Applicative[F]): FreeApplicative[DynamoOpA, ?] ~> F = new (FreeApplicative[DynamoOpA, ?] ~> F) {
        def apply[A](fa: FreeApplicative[DynamoOpA,A]): F[A] = fa.foldMap(interpreter)
    }

    class DynamoOpSyntax[A](op: DynamoOp[A]) {
        def eval[F[_]: Monad](interpreter: DynamoOpA ~> F): F[A] = 
            op.foldMap(parallelInterpreter(interpreter))
        def evalP[F[_]: Monad: Parallel](interpreter: DynamoOpA ~> F)(implicit P: Parallel[F]) = 
            op.foldMap(parallelInterpreter(interpreter.andThen(P.parallel))(P.applicative).andThen(P.sequential))
    }

    class FreeApplicativeSyntax[A](fa: FreeApplicative[DynamoOpA, A]) {
        def seq: DynamoOp[A] = Free.liftF(fa)
    }
}