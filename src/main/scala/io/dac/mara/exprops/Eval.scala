package io.dac.mara.exprops

import io.dac.mara.core.{Expr, MaraValue}
import io.dac.mara.core.Expr.Family

/**
  * Created by dcollins on 8/2/16.
  */


case class Eval(eval: MaraValue) extends Expr

object Eval {
  implicit object EvalFamily extends Family[Eval, MaraValue] {
    override def value(e: Eval) = e.eval
  }
}


trait EvalOp {
  def op(f: => MaraValue) = Eval(f)
}
