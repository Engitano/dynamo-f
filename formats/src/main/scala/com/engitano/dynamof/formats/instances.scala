package com.engitano.dynamof.formats

import cats.Contravariant
import cats.Functor

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

trait MarshallerInstances extends FromDynamoValueInstances with ToDynamoValueInstances

