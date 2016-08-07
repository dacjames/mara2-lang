package io.dac.mara.impls

import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.operators.OperatorAlg

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalOperator extends EvalOp with OperatorAlg[Eval] {
  implicit def int2bool(x: Int): Boolean = if (x == 0) false else true
  implicit def bool2int(x: Boolean): Int = if (x) { 1 } else { 0 }

  override def lt(x: Eval, y: Eval) = op { x.eval < y.eval }
  override def gt(x: Eval, y: Eval) = op { x.eval > y.eval }
  override def lte(x: Eval, y: Eval) = op { x.eval <= y.eval }
  override def gte(x: Eval, y: Eval) = op { x.eval >= y.eval }
  override def and(x: Eval, y: Eval) = op { x.eval && y.eval }
  override def or(x: Eval, y: Eval) = op { x.eval || y.eval }

  override def plus(x: Eval, y: Eval) = op { x.eval + y.eval }
  override def minus(x: Eval, y: Eval) = op { x.eval - y.eval }
  override def times(x: Eval, y: Eval) = op { x.eval * y.eval }
  override def divide(x: Eval, y: Eval) = op { x.eval / y.eval }
  override def power(x: Eval, y: Eval) = op { Math.pow(x.eval, y.eval).toInt }
}

