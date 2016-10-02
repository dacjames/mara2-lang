package io.dac.mara.lang.controlflow

import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.core.MaraValue

/**
  * Created by dcollins on 8/6/16.
  */
trait EvalControlFlow extends EvalOp with ControlFlowAlg[Eval] {
  import MaraValue._
  import MaraValue.implicits.truthy._

  override def ifx(pred: Eval, body: Eval) = op {
    if (pred.eval) { body.eval } else { UnitValue() }
  }

  override def elsex(expr: Eval, otherwise: Eval) = op {
    val evaled = expr.eval
    evaled match {
      case UnitValue() => otherwise.eval
      case _ => if (evaled) { evaled } else otherwise.eval
    }

  }

}
