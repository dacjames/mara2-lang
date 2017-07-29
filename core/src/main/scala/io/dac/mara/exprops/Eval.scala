package io.dac.mara.exprops

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */


trait Eval extends Expr[Eval] {
  override type Target = MaraValue
  override def value: MaraValue = eval

  def eval: MaraValue
}

object Eval {
  implicit object EvalPhaseKey extends PhaseKey[Eval] {
    override def key: Int = 2
  }
}


trait EvalOp extends ExprOps[Eval] {
  def op(f: => MaraValue) = {
    new Eval {
      override def eval = f
      override val phase: Phase = context.nextPhase[Eval]
    }
  }
}


