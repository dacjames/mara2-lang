package io.dac.mara.lang.controlflow

import io.dac.mara.core.{MaraValue, NamespaceLookup}
import io.dac.mara.phases.{Eval, EvalOp}

/**
  * Created by dcollins on 8/6/16.
  */
trait EvalControlFlow extends EvalOp with ControlFlowAlg[Eval] with NamespaceLookup {
  import MaraValue._
  import MaraValue.implicits.truthy._

  override def ifx(pred: Eval, body: Eval) = op {
    if (pred.eval) {
      inNewScope {
        body.eval
      }
    } else {
      UnitValue()
    }
  }

  override def elsex(expr: Eval, otherwise: Eval) = op {
    val evaled = expr.eval
    inNewScope {
      evaled match {
        case UnitValue() => otherwise.eval
        case _ =>
          if (evaled) { evaled }
          else otherwise.eval
      }
    }

  }

}
