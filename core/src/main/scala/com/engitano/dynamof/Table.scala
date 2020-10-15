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

import com.engitano.dynamof.syntax._
import shapeless.ops.record.Selector
import shapeless.{=:!=, HList, LabelledGeneric, Witness}
import com.engitano.dynamof.Index.IndexType
import com.engitano.dynamof.Index.Local
import com.engitano.dynamof.Index.Global

sealed trait Index[A]{
  val tableName: String
  val indexName: String
  val key: PrimaryKey
  type KeyId
  type KeyValue
  type Type <: IndexType
}

object Index {

  sealed trait IndexType
  case class Local() extends IndexType
  case class Global() extends IndexType

  type Aux[A0, KID, KV, IXT <: IndexType] = Index[A0] {
    type KeyId    = KID
    type KeyValue = KV
    type Type = IXT
  }
}

sealed trait Table[A] {
  val name: String
  val key: PrimaryKey
  type KeyId
  type KeyValue

  def localSecondaryIndex[HK <: Symbol, HV, RK0, RK <: Symbol, RV, Repr <: HList](ixName: String, rk: Witness.Aux[RK])(
        implicit
        hk: KeyId <:< (HK, RK0),
        kv: KeyValue <:< (HV, _),
        ev: RK0 =:!= RK,
        hw: Witness.Aux[HK],
        hsat: ToScalarAttr[HV],
        gen: LabelledGeneric.Aux[A, Repr],
        rs: Selector.Aux[Repr, RK, RV],
        rsat: ToScalarAttr[RV]
    ): Index.Aux[A, (HK, RK), (HV, RV), Local] = new Index[A] {
      val indexName = ixName
      val tableName: String = name
      val key: PrimaryKey  = CompositeKey(AttributeDefinition(hw.value.name, hsat.to), AttributeDefinition(rk.value.name, rsat.to))
      type KeyId    = (HK, RK)
      type KeyValue = (HV, RV)
      type Type = Local
    }

  def globalSecondaryIndex[HK <: Symbol, HV, RK <: Symbol, RV, Repr <: HList](ixName: String, hk: Witness.Aux[HK], rk: Witness.Aux[RK])(
        implicit
        ev: (HK, RK) =:!= KeyId,
        ev1: HK =:!= RK,
        gen: LabelledGeneric.Aux[A, Repr],
        hs: Selector.Aux[Repr, HK, HV],
        hsat: ToScalarAttr[HV],
        rs: Selector.Aux[Repr, RK, RV],
        rsat: ToScalarAttr[RV]
    ): Index.Aux[A, (HK, RK), (HV, RV), Global] = new Index[A] {
      val indexName = ixName
      val tableName: String = name
      val key: PrimaryKey  = CompositeKey(AttributeDefinition(hk.value.name, hsat.to), AttributeDefinition(rk.value.name, rsat.to))
      type KeyId    = (HK, RK)
      type KeyValue = (HV, RV)
      type Type = Global
    }
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
        sat: ToScalarAttr[V]
    ): Table.Aux[A, K, V] = new Table[A] {
      val name           = tableName
      val key: PrimaryKey = SimpleKey(AttributeDefinition(hk.value.name, sat.to))
      type KeyId    = K
      type KeyValue = V
    }
    
    def apply[HK <: Symbol, HV, RK <: Symbol, RV, Repr <: HList](tableName: String, hk: Witness.Aux[HK], rk: Witness.Aux[RK])(
        implicit
        ev: HK =:!= RK,
        gen: LabelledGeneric.Aux[A, Repr],
        hs: Selector.Aux[Repr, HK, HV],
        hsat: ToScalarAttr[HV],
        rs: Selector.Aux[Repr, RK, RV],
        rsat: ToScalarAttr[RV]
    ): Table.Aux[A, (HK, RK), (HV, RV)] = new Table[A] {
      val name = tableName
      val key: PrimaryKey  = CompositeKey(AttributeDefinition(hk.value.name, hsat.to), AttributeDefinition(rk.value.name, rsat.to))
      type KeyId    = (HK, RK)
      type KeyValue = (HV, RV)
    }
  }

  def apply[A]: TableDefPartiallyApplied[A] = TableDefPartiallyApplied[A]()

}

