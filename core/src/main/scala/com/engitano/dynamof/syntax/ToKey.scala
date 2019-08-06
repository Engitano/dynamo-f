package com.engitano.dynamof.syntax

import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.Witness

sealed trait ToKey[K, A] {
  def toKey(a: A): DynamoValue.M
}

trait ToKeySyntax {

  implicit def toHashKeyT[K <: Symbol, V: ToDynamoValue](implicit w: Witness.Aux[K]) = new ToKey[K, V] {
    def toKey(a: V): DynamoValue.M = DynamoValue.M(Map(w.value.name -> ToDynamoValue[V].to(a)))
  }
  implicit def toHashKeyOtherT[HK <: Symbol, RK <: Symbol, HV: ToDynamoValue, RV: ToDynamoValue]
    (implicit h: Witness.Aux[HK], r: Witness.Aux[RK]) = new ToKey[(HK, RK), (HV, RV)] {
    def toKey(a: (HV, RV)): DynamoValue.M =
      DynamoValue.M(
        Map(
          h.value.name  -> ToDynamoValue[HV].to(a._1),
          r.value.name -> ToDynamoValue[RV].to(a._2)
        )
      )
  }
}

// trait IsPrimaryKey[F,V] {
//   def toM(v: V): DynamoValue.M
// }

// trait IsCompoundKey[KF, KV] extends IsPrimaryKey[KF, KV] {
//   type HF
//   type HV
//   type RF
//   type RV
// }

// object IsCompoundKey {
//   type Aux[KF, KV, HF0, HV0, RF0, RV0] = IsCompoundKey[KF, KV] {
//     type HF = HF0
//     type HV = HV0
//     type RF = RF0
//     type RV = RV0
//   }

//   implicit def apply[KF, KV, HF0 <: Symbol, HV0, RF0 <: Symbol, RV0]
//     (implicit 
//       keyEv: KF =:= (HF0, RF0),
//       valueEv: KV =:= (HV0, RV0),
//       hkW: Witness.Aux[HF0],
//       rkW: Witness.Aux[RF0],
//       tdvH: ToDynamoValue[HV0],
//       tdvR: ToDynamoValue[RV0]
//     ): IsCompoundKey.Aux[KF, KV, HF0, HV0, RF0, RV0] = new IsCompoundKey[KF,KV] {
//       type HF = HF0
//       type HV = HV0
//       type RF = RF0
//       type RV = RV0
//       def toM(v: KV): DynamoValue.M = 
//       DynamoValue.M(
//         Map(
//           hkW.value.name  -> ToDynamoValue[HV].to(valueEv(v)._1),
//           rkW.value.name -> ToDynamoValue[RV].to(valueEv(v)._2)
//         )
//       )
//     }
// }