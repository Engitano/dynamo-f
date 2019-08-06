package com.engitano.dynamof.syntax

import shapeless._
import com.engitano.dynamof.formats.`package`.NonEmptyString
import shapeless.ops.hlist.Prepend


object SymbolOps {
    type Aux[K0] = SymbolOps { type K = Witness.Aux[K0] }
  }
  
  trait SymbolOps {
    import shapeless.labelled._
    type K
    def <[KV](v: KV): FieldType[K, FieldPredicate[KV]] :: HNil   = field[K](lt[KV](v)) :: HNil
    def <=[KV](v: KV): FieldType[K, FieldPredicate[KV]] :: HNil  = field[K](lte[KV](v)) :: HNil
    def ===[KV](v: KV): FieldType[K, FieldPredicate[KV]] :: HNil = field[K](equalTo[KV](v)) :: HNil
    def >=[KV](v: KV): FieldType[K, FieldPredicate[KV]] :: HNil  = field[K](gte[KV](v)) :: HNil
    def >[KV](v: KV): FieldType[K, FieldPredicate[KV]] :: HNil   = field[K](gt[KV](v)) :: HNil
    def beginsWith[KV](v: KV)(implicit isS: KV =:= NonEmptyString): FieldType[K, FieldPredicate[KV]] :: HNil =
      field[K](com.engitano.dynamof.syntax.beginsWith[KV](v)) :: HNil
  }
  
  trait FilterSyntax {
    import scala.language.experimental.macros
    implicit def symbolOps(s: Symbol): SymbolOps = macro FilterMacros.filterOps
  
    implicit class FilterOpExtensions[LHS <: HList](l: LHS)(implicit ev1: ToPredicate[LHS]) {
      def and[RHS <: HList](r: RHS)(implicit ev1: ToPredicate[RHS], p: Prepend[LHS, RHS]) = l ++ r
    }
  }
  