package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait ExprOp[A, B] {
  def op(f: => A): B
}

