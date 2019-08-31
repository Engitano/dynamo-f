package com.engitano.dynamof.syntax

import shapeless._
import shapeless.labelled.FieldType
import com.engitano.dynamof.Predicate
import com.engitano.dynamof.formats.ToDynamoValue
import com.engitano.dynamof.{ And => Amp }

trait ToPredicate[L <: HList] {
    def to(l: L): Option[Predicate]
  }
  
  trait ToPredicateInstances {
  
    implicit def toPredicateHNil[T <: HNil]: ToPredicate[T] = new ToPredicate[T] {
      def to(l: T): Option[Predicate] = None
    }
  
    implicit def toPredicateHCons[K <: Symbol, KV, T <: HList](
        implicit key: Witness.Aux[K],
        tp: ToPredicate[T],
        tdv: ToDynamoValue[KV]
    ): ToPredicate[FieldType[K, FieldPredicate[KV]] :: T] =
      new ToPredicate[FieldType[K, FieldPredicate[KV]] :: T] {
        def to(l: FieldType[K, FieldPredicate[KV]] :: T): Option[Predicate] = {
          val and = tp.to(l.tail)
          val h   = l.head.toPredicate(key.value.name)
          and match {
            case Some(a) => Some(Amp(a, h))
            case None    => Some(h)
          }
        }
      }
  }
  