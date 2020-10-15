package com.engitano.dynamof.syntax

import cats.free.FreeApplicative
import com.engitano.dynamof.{ DynamoOpA, DynamoOp }
import cats.free.Free

trait FreeApplicativeOpsSyntax {
    implicit def toFreeApplicativeSyntax[A](op: FreeApplicative[DynamoOpA, A]) = new FreeApplicativeSyntax(op)
}

class FreeApplicativeSyntax[A](fa: FreeApplicative[DynamoOpA, A]) {
    def seq: DynamoOp[A] = Free.liftF(fa)
}