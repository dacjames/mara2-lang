package io.dac.mara.lang.literals

import io.dac.mara.exprops.{Show, ShowOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait ShowLiteral extends ShowOp with LiteralAlg[Show] {
  override def litInt(it: Int) = op { it.toString }
  override def litString(it: String) = op { s"'$it'" }
  override def litbool(it: Boolean) = op { it.toString }
}
