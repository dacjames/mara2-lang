package io.dac.mara.exprops

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */
trait Show extends Expr[Show] {
  override type Target = String
  override def value: String = show

  def show: String
}

object Show {
  implicit object ShowPhaseKey extends PhaseKey[Show] {
    override def key: Int = 0
  }
}

trait ShowOp extends ExprOps[Show] {
  def op(f: => String) = new Show {
    override def show = f
    override val phase: Phase = context.nextPhase[Show]
  }
}
