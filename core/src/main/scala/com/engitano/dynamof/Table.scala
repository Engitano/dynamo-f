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
import com.engitano.dynamof.syntax._
import com.engitano.dynamof.formats.ToDynamoValue
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import shapeless.labelled._
import shapeless.ops.record.Selector
import shapeless.{=:!=, HList, LabelledGeneric, Witness, Generic, ::, HNil}
import cats.effect.Async
import java.util.concurrent.CompletableFuture
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType



sealed trait Table[A] {
  val name: String
  val key: KeyId
  type KeyId
  type KeyValue
}

object Table {

  type Aux[A0, KID, KV] = Table[A0] {
    type KeyId    = KID
    type KeyValue = KV
  }

  case class TableDefPartiallyApplied[A](dummy: Boolean = false) extends AnyVal {
    def apply[K <: Symbol, V, Repr <: HList](tableName: String, hk: Witness.Aux[K])(
        implicit
        gen: LabelledGeneric.Aux[A, Repr],
        s: Selector.Aux[Repr, K, V],
        sat: HasScalarAttributeRepr[V]
    ): Table.Aux[A, SimpleKey, V] = new Table[A] {
      val name           = tableName
      val key: SimpleKey = SimpleKey(AttributeDefinition(hk.value.name, sat.to))
      type KeyId    = SimpleKey
      type KeyValue = V
    }
    def apply[HK <: Symbol, HV, RK <: Symbol, RV, Repr <: HList](tableName: String, hk: Witness.Aux[HK], rk: Witness.Aux[RK])(
        implicit
        gen: LabelledGeneric.Aux[A, Repr],
        hs: Selector.Aux[Repr, HK, HV],
        hsat: HasScalarAttributeRepr[HV],
        rs: Selector.Aux[Repr, RK, RV],
        rsat: HasScalarAttributeRepr[RV]
    ): Table.Aux[A, CompositeKey, (HV, RV)] = new Table[A] {
      val name = tableName
      val key  = CompositeKey(AttributeDefinition(hk.value.name, hsat.to), AttributeDefinition(rk.value.name, rsat.to))
      type KeyId    = CompositeKey
      type KeyValue = (HV, RV)
    }
  }

  def apply[A]: TableDefPartiallyApplied[A] = TableDefPartiallyApplied[A]()

}

