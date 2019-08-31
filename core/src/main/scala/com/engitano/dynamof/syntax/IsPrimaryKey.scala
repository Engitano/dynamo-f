package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.Witness

sealed trait IsPrimaryKey[F, V] {
  def primaryKey(v: V): DynamoValue.M
  def primaryKeyDefinition: PrimaryKey
}

sealed trait IsSimpleKey[K, V] extends IsPrimaryKey[K, V]

trait IsCompositeKey[KF, KV] extends IsPrimaryKey[KF, KV] {
  type HF
  type HV
  type RF
  type RV
  def primaryKey(v: (HV, RV)): DynamoValue.M
  def hashKey(v: HV): DynamoValue.M
  def rangeKey(v: RV): DynamoValue.M
}

object IsCompositeKey {
  type Aux[KF, KV, HF0, HV0, RF0, RV0] = IsCompositeKey[KF, KV] {
    type HF = HF0
    type HV = HV0
    type RF = RF0
    type RV = RV0
  }

}

trait IsPrimaryKeySyntax {

  implicit def isSimpleKey[K <: Symbol, V](implicit w: Witness.Aux[K], tdv: ToDynamoValue[V], hsa: ToScalarAttr[V]) = new IsSimpleKey[K, V] {
    def primaryKey(v: V): DynamoValue.M =
      DynamoValue.M(
        Map(
          w.value.name -> ToDynamoValue[V].to(v)
        )
      )
    def primaryKeyDefinition: PrimaryKey = {
      SimpleKey(AttributeDefinition(w.value.name, hsa.to))
    }
  }

  implicit def isCompositeKey[KF, KV, HF0 <: Symbol, HV0, RF0 <: Symbol, RV0](
      implicit
      keyEv: KF <:< (HF0, RF0),
      valueEv: KV <:< (HV0, RV0),
      hkW: Witness.Aux[HF0],
      rkW: Witness.Aux[RF0],
      tdvH: ToDynamoValue[HV0],
      tdvR: ToDynamoValue[RV0],
      tsaH: ToScalarAttr[HV0],
      tsaR: ToScalarAttr[RV0]
  ): IsCompositeKey.Aux[KF, KV, HF0, HV0, RF0, RV0] = new IsCompositeKey[KF, KV] {
    type HF = HF0
    type HV = HV0
    type RF = RF0
    type RV = RV0
    def primaryKey(v: KV): DynamoValue.M =
      DynamoValue.M(
        Map(
          hkW.value.name -> ToDynamoValue[HV].to(valueEv(v)._1),
          rkW.value.name -> ToDynamoValue[RV].to(valueEv(v)._2)
        )
      )
    def primaryKeyDefinition: PrimaryKey = {
      CompositeKey(
        AttributeDefinition(hkW.value.name, tsaH.to),
        AttributeDefinition(rkW.value.name, tsaR.to)
      )
    }

    def primaryKey(v: (HV0, RV0)): DynamoValue.M =
      DynamoValue.M(
        Map(
          hkW.value.name -> ToDynamoValue[HV].to(v._1),
          rkW.value.name -> ToDynamoValue[RV].to(v._2)
        )
      )

    def hashKey(v: HV): DynamoValue.M =
      DynamoValue.M(
        Map(
          hkW.value.name -> ToDynamoValue[HV].to(v)
        )
      )

    def rangeKey(v: RV): DynamoValue.M =
      DynamoValue.M(
        Map(
          rkW.value.name -> ToDynamoValue[RV].to(v)
        )
      )
  }
}
