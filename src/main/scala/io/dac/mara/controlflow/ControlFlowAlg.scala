package io.dac.mara.controlflow

import io.dac.mara.core.ExprAlg

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowAlg[E] extends ExprAlg[E] {
  def ifx(pred: E, body: E): E
  def elsex(expr: E, otherwise: E): E
}
