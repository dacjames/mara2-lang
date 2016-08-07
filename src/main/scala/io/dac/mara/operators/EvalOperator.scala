package io.dac.mara.operators

import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalOperator extends EvalOp with OperatorAlg[Eval] {
  import io.dac.mara.core.IntBoolConverters._

  override def lt(x: Eval, y: Eval) = op { x.eval < y.eval }
  override def gt(x: Eval, y: Eval) = op { x.eval > y.eval }
  override def lte(x: Eval, y: Eval) = op { x.eval <= y.eval }
  override def gte(x: Eval, y: Eval) = op { x.eval >= y.eval }
  override def ne(x: Eval, y: Eval) = op { x.eval != y.eval }

  override def and(x: Eval, y: Eval) = op { x.eval && y.eval }
  override def or(x: Eval, y: Eval) = op { x.eval || y.eval }
  override def not(x: Eval) = op { ! x.eval }

  override def plus(x: Eval, y: Eval) = op { x.eval + y.eval }
  override def minus(x: Eval, y: Eval) = op { x.eval - y.eval }
  override def times(x: Eval, y: Eval) = op { x.eval * y.eval }
  override def divide(x: Eval, y: Eval) = op { x.eval / y.eval }
  override def power(x: Eval, y: Eval) = op { Math.pow(x.eval, y.eval).toInt }
}

