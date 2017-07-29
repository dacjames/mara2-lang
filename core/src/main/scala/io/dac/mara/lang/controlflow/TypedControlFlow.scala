package io.dac.mara.lang.controlflow

import io.dac.mara.core.MaraType.AnyType
import io.dac.mara.phases.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedControlFlow extends TypedOp with ControlFlowAlg[Typed] {
  override def ifx(pred: Typed, body: Typed): Typed = op {
    AnyType()
  }

  override def elsex(expr: Typed, otherwise: Typed): Typed = op {
    AnyType()
  }
}
