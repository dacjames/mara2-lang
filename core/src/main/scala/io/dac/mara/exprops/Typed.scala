package io.dac.mara.exprops

import io.dac.mara.core._

/**
  * Created by dcollins on 8/27/16.
  */


trait Typed extends Expr[Typed] {
  override type Target = MaraType
  override def value: MaraType = typex

  def typex: MaraType
}

object Typed {
  implicit object TypedPhaseKey extends PhaseKey[Typed] {
    override def key: Int = 1
  }
}

trait TypedOp extends ExprOps[Typed] {
  def op(f: => MaraType) = new Typed {
    override def typex = f
    override val phase: Phase = context.nextPhase[Typed]
  }
}

