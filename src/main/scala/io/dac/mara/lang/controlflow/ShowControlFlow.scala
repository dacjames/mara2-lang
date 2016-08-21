package io.dac.mara.lang.controlflow
import io.dac.mara.exprops.{Show, ShowOp}

/**
  * Created by dcollins on 8/6/16.
  */
trait ShowControlFlow extends ShowOp with ControlFlowAlg[Show] {
  override def ifx(pred: Show, body: Show) = op {
    s"if ${pred.show} { ${body.show} }"
  }

  override def elsex(expr: Show, otherwise: Show) = op {
    s"${expr.show} else { ${otherwise.show} }"
  }

}