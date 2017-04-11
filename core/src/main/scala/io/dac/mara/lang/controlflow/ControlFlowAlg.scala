package io.dac.mara.lang.controlflow

import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowAlg[E] extends LangAlg[E] {
  def ifx(pred: E, body: E): E
  def elsex(expr: E, otherwise: E): E
}