package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.Witness

sealed trait IsPrimaryKey[F,V] {
  def primaryKey(v: V): DynamoValue.M
}

sealed trait IsSimpleKey[K, V] extends IsPrimaryKey[K, V]

trait IsCompoundKey[KF, KV]  extends IsPrimaryKey[KF, KV]{
  type HF
  type HV
  type RF
  type RV
  def hashKey(v: HV): DynamoValue.M
  def rangeKey(v: RV): DynamoValue.M
}

object IsCompoundKey {
  type Aux[KF, KV, HF0, HV0, RF0, RV0] = IsCompoundKey[KF, KV] {
    type HF = HF0
    type HV = HV0
    type RF = RF0
    type RV = RV0
  }

}

trait IsPrimaryKeySyntax {

  implicit def isSimpleKey[K <: Symbol, V](implicit w: Witness.Aux[K], tdv: ToDynamoValue[V]) = new IsSimpleKey[K, V] {
    def primaryKey(v: V): DynamoValue.M = 
    DynamoValue.M(
      Map(
        w.value.name  -> ToDynamoValue[V].to(v)
      )
    )
  }

  implicit def isCompoundKey[KF, KV, HF0 <: Symbol, HV0, RF0 <: Symbol, RV0]
    (implicit 
      keyEv: KF <:< (HF0, RF0),
      valueEv: KV <:< (HV0, RV0),
      hkW: Witness.Aux[HF0],
      rkW: Witness.Aux[RF0],
      tdvH: ToDynamoValue[HV0],
      tdvR: ToDynamoValue[RV0]
    ): IsCompoundKey.Aux[KF, KV, HF0, HV0, RF0, RV0] = new IsCompoundKey[KF,KV] {
      type HF = HF0
      type HV = HV0
      type RF = RF0
      type RV = RV0
      def primaryKey(v: KV): DynamoValue.M = 
        DynamoValue.M(
          Map(
            hkW.value.name  -> ToDynamoValue[HV].to(valueEv(v)._1),
            rkW.value.name -> ToDynamoValue[RV].to(valueEv(v)._2)
          )
        )

      def hashKey(v: HV): DynamoValue.M = 
        DynamoValue.M(
          Map(
            hkW.value.name  -> ToDynamoValue[HV].to(v)
          )
        )

      def rangeKey(v: RV): DynamoValue.M = 
        DynamoValue.M(
          Map(
            rkW.value.name  -> ToDynamoValue[RV].to(v)
          )
        )
    }
}