package io.dac.mara.lang.literals

import io.dac.mara.phases.{Show, ShowOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait ShowLiteral extends ShowOp with LiteralAlg[Show] {
  override def litint(it: Int) = op { it.toString }
  override def litstring(it: String) = op { s"'$it'" }
  override def litbool(it: Boolean) = op { it.toString }
}
