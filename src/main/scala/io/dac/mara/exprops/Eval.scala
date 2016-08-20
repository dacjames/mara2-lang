package io.dac.mara.exprops

import io.dac.mara.core.Expr
import io.dac.mara.core.Expr.Family

/**
  * Created by dcollins on 8/2/16.
  */


case class Eval(val eval: Int) extends Expr

object Eval {
  implicit object EvalFamily extends Family[Eval, Int] {
    override def value(e: Eval) = e.eval
  }
}


trait EvalOp {
  def op(f: => Int) = Eval(f)
}
