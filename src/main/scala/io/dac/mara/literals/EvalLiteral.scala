package io.dac.mara.literals

import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalLiteral extends EvalOp with LiteralAlg[Eval] {
  override def litInt(it: Int): Eval = op { it }
  override def litString(it: String): Eval = op { ??? }
}
