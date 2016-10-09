package io.dac.mara.exprops

import io.dac.mara.core.Expr.Family
import io.dac.mara.core.{Expr, MaraType}

/**
  * Created by dcollins on 8/27/16.
  */


//trait Eval extends Expr {
//  def eval: MaraValue
//}

trait Typed extends Expr {
  def typex: MaraType
}

object Typed {
  implicit object TypedFamily extends Family[Typed, MaraType] {
    override def value(e: Typed) = e.typex
  }
}

trait TypedOp {
  def op(f: => MaraType) = new Typed {
    override def typex = f
  }
}

