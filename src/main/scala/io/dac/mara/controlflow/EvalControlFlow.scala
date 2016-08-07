package io.dac.mara.controlflow

import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/6/16.
  */
trait EvalControlFlow extends EvalOp with ControlFlowAlg[Eval] {
  import io.dac.mara.core.IntBoolConverters._

  override def ifx(pred: Eval, body: Eval) = op {
    if (pred.eval) { body.eval } else { 0 }
  }

  override def elsex(expr: Eval, otherwise: Eval) = op {
    val evaled = expr.eval
    if (evaled == 0) { otherwise.eval } else evaled
  }

}
