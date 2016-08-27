package io.dac.mara.exprops

import io.dac.mara.core.Expr.Family
import io.dac.mara.core.{Expr, MaraType}

/**
  * Created by dcollins on 8/27/16.
  */


case class Typed(val typex: MaraType) extends Expr

object Typed {
  implicit object TypedFamily extends Family[Typed, MaraType] {
    override def value(e: Typed) = e.typex
  }
}

trait TypedOp {
  def op(f: => MaraType) = Typed(f)
}

