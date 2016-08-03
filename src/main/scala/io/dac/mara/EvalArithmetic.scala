package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalArithmetic extends EvalOp with ArithmeticAlg[Eval] {
  override def plus(x: Eval, y: Eval): Eval = op { x.eval + y.eval }
  override def minus(x: Eval, y: Eval): Eval = op { x.eval - y.eval }
  override def times(x: Eval, y: Eval): Eval = op { x.eval * y.eval }
  override def divide(x: Eval, y: Eval): Eval = op { x.eval / y.eval }
  override def power(x: Eval, y: Eval): Eval = op { Math.pow(x.eval, y.eval).toInt }
}