package io.dac.mara.lang.operators

import io.dac.mara.core.MaraValue
import io.dac.mara.core.MaraValue._
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalOperator extends EvalOp with OperatorAlg[Eval] {
  import MaraValue.implicits._
  import MaraValue.implicits.truthy._

  private[this] def intop(xv: => MaraValue, yv: => MaraValue)(f: (Int, Int) => MaraValue) = op {
    (xv, yv) match {
      case (IntValue(x), IntValue(y)) => f(x, y)
      case _ => ErrorValue(s"Type Error: ${xv} or ${yv} is not of type Int")
    }
  }


  override def lt(x: Eval, y: Eval) = intop(x.eval, y.eval){ _ < _ }
  override def gt(x: Eval, y: Eval) = intop(x.eval, y.eval){ _ > _ }
  override def lte(x: Eval, y: Eval) = intop(x.eval, y.eval){ _ <= _ }
  override def gte(x: Eval, y: Eval) = intop(x.eval, y.eval){ _ >= _ }
  override def ne(x: Eval, y: Eval) = op { x.eval != y.eval }
  override def eq(x: Eval, y: Eval) = op { x.eval == y.eval }

  override def and(x: Eval, y: Eval) = op { x.eval && y.eval }
  override def or(x: Eval, y: Eval) = op { x.eval || y.eval }
  override def not(x: Eval) = op { ! x.eval }

  override def plus(x: Eval, y: Eval) = intop(x.eval, y.eval) { _ + _ }
  override def minus(x: Eval, y: Eval) = intop(x.eval, y.eval) { _ - _ }
  override def times(x: Eval, y: Eval) = intop(x.eval, y.eval) { _ * _ }
  override def divide(x: Eval, y: Eval) = intop(x.eval, y.eval) { _ / _ }
  override def power(x: Eval, y: Eval) = intop(x.eval, y.eval) { Math.pow(_, _).toInt }
}

