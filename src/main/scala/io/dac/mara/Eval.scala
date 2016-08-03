package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */


case class Eval(val eval: Int)


trait EvalOp {
  def op(f: => Int) = Eval(f)
}
