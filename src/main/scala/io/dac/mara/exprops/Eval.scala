package io.dac.mara.exprops

/**
  * Created by dcollins on 8/2/16.
  */


case class Eval(val eval: Int)


trait EvalOp {
  def op(f: => Int) = Eval(f)
}
