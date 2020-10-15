package com.engitano.dynamof.formats

import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import scala.reflect.macros.blackbox


class NonEmptyStringMacros(val c: blackbox.Context) {
  import c.universe._

  def nesImpl(args: c.Expr[Any]*): c.Expr[NonEmptyString] = c.prefix.tree match {
    case Apply(_, Seq(Apply(_, parts))) =>
      if (parts.length != args.length + 1)
        abort("wrong number of arguments (" + args.length + ") for interpolated string with " +
          parts.length + " parts")
      
      if (parts forall { case Literal(Constant("")) => true ; case _ => false })
        if (parts.size == 1)
          abort("Cannot create a NonEmptyString with the empty string")
        else
          abort("Cannot create a NonEmptyString with possibly an empty interpolated string")

      c.Expr[NonEmptyString](q"""
      val parts = _root_.scala.List[_root_.java.lang.String](..$parts)
      val args = _root_.scala.List[_root_.scala.Any](..$args)
      val pi = parts.iterator
      val ai = args.iterator
      val bldr = new _root_.java.lang.StringBuilder(_root_.scala.StringContext processEscapes pi.next())
      while (ai.hasNext) {
        bldr append ai.next()
        bldr append (_root_.scala.StringContext processEscapes pi.next())
      }
      val res = bldr.toString
      _root_.eu.timepit.refined.types.string.NonEmptyString.unsafeFrom(res)
    """)
  }

  def abort(msg: String) = c.abort(c.enclosingPosition, msg)
}