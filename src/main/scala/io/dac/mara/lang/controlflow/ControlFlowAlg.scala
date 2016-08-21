package io.dac.mara.lang.controlflow

import io.dac.mara.core.{Expr, ExprAlg}

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowAlg[E <: Expr] extends ExprAlg[E] {
  def ifx(pred: E, body: E): E
  def elsex(expr: E, otherwise: E): E
}
