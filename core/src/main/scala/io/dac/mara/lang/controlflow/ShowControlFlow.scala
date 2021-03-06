package io.dac.mara.lang.controlflow

import io.dac.mara.phases.{Show, ShowOp}

/**
  * Created by dcollins on 8/6/16.
  */
trait ShowControlFlow extends ShowOp with ControlFlowAlg[Show] {
  override def ifx(pred: Show, body: Show) = op {
    s"if ${pred.show} { ${body.show} }"
  }

  override def elsex(expr: Show, otherwise: Show) = op {
    val body = otherwise.show
    if (body.replace(" ", "").length == 0) expr.show
    else s"${expr.show} else { ${otherwise.show} }"
  }

}