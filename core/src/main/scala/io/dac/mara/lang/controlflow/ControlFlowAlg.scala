package io.dac.mara.lang.controlflow

import io.dac.mara.core._

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowAlg[E] extends ExprAlg[E] {
  def ifx(pred: E, body: E): E = empty
  def elsex(expr: E, otherwise: E): E = empty
}
