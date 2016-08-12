package io.dac.mara.exprops

import io.dac.mara.core.Expr

/**
  * Created by dcollins on 8/2/16.
  */


case class Eval(val eval: Int) extends Expr


trait EvalOp {
  def op(f: => Int) = Eval(f)
}
