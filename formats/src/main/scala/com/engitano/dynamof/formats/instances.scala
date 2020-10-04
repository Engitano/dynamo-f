package com.engitano.dynamof.formats

import cats.syntax.functor._
import cats.Contravariant
import cats.Functor
import cats.data.Validated

trait FromDynamoValueInstances {

    implicit def catsDataFunctorForFromDynamoValue = new Functor[FromDynamoValue] {
        def map[A, B](fa: FromDynamoValue[A])(f: A => B): FromDynamoValue[B] = new FromDynamoValue[B] {
            def from(av: DynamoValue): Either[DynamoUnmarshallException, B] = fa.from(av).map(f)
        }
    }
}

trait ToDynamoValueInstances {

    implicit def catsDataContravariantForToDynamoValue = new Contravariant[ToDynamoValue] {
        def contramap[A, B](fa: ToDynamoValue[A])(f: B => A): ToDynamoValue[B] = new ToDynamoValue[B]{
            def to(a: B): DynamoValue = fa.to(f(a))
        }
    }
}

object instances {
    object fromDynamoValue extends FromDynamoValueInstances
    object toDynamoValue extends ToDynamoValueInstances
    object all extends FromDynamoValueInstances with ToDynamoValueInstances
}