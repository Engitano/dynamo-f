package com.engitano.dynamof.syntax

import cats.instances.either._
import cats.syntax.apply._
import com.engitano.dynamof._
import com.engitano.dynamof.formats._
import shapeless.Witness
import shapeless.HNil

sealed trait IsPrimaryKey[K, V] {
  def primaryKey(v: V): DynamoValue.M
  def primaryKeyDefinition: PrimaryKey
  def parseKey(m: DynamoValue.M): Either[DynamoUnmarshallException, V]
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

  implicit def isSimpleKey[K <: Symbol, V](
      implicit w: Witness.Aux[K],
      tdv: ToDynamoValue[V],
      fdv: FromDynamoValue[V],
      hsa: ToScalarAttr[V]
  ) =
    new IsSimpleKey[K, V] {

      override def parseKey(m: DynamoValue.M): Either[DynamoUnmarshallException, V] = {
         m.m.get(w.value.name).toRight(AttributeNotFoundException(w.value.name)).flatMap(fdv.from)
      }

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
      fdvH: FromDynamoValue[HV0],
      fdvR: FromDynamoValue[RV0],
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
    def parseKey(m: DynamoValue.M): Either[DynamoUnmarshallException, KV] = {
      val hk = m.m.get(hkW.value.name).toRight(AttributeNotFoundException(hkW.value.name)).flatMap(fdvH.from)
      val rk = m.m.get(rkW.value.name).toRight(AttributeNotFoundException(rkW.value.name)).flatMap(fdvR.from)
      (hk, rk).tupled.map(_.asInstanceOf[KV])
    }
  }
}
