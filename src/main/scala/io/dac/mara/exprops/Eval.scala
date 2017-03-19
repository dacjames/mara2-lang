package io.dac.mara.exprops

import io.dac.mara.core.{Expr, MaraValue}
import io.dac.mara.core.Expr.Replable

/**
  * Created by dcollins on 8/2/16.
  */


trait Eval extends Expr {
  def eval: MaraValue
}

object Eval {
  implicit object EvalReplable$ extends Replable[Eval, MaraValue] {
    override def value(e: Eval) = e.eval
  }
}


trait EvalOp {
  def op(f: => MaraValue) = {
    new Eval {
      override def eval = f
    }
  }
}


