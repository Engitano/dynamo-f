/*
 * Copyright (c) 2019 Engitano
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.engitano.dynamof

import com.engitano.dynamof.formats._
import com.engitano.dynamof.formats.auto._
import cats.Functor
import cats.effect.{ Resource, Bracket }

object testSyntax extends DynamoFClientSyntax

trait DynamoFClientSyntax {

    implicit def toTestOps[F[_]](dynamoFClient: DynamoFClient[F]) = new DynamoFClientTestOps(dynamoFClient)

    class DynamoFClientTestOps[F[_]](dynamoFClient: DynamoFClient[F]) {
        def createTableResource(createTableRequest: CreateTableRequest)(implicit F: Functor[F]) = 
            Resource.make(dynamoFClient.createTable(createTableRequest))(_ => dynamoFClient.deleteTable(createTableRequest.name))
        def useTable[A](createTableRequest: CreateTableRequest)(f: => F[A])(implicit F: Bracket[F, Throwable]) = 
            createTableResource(createTableRequest).use(_ => f)
    }
}