package io.dac.mara.lang.literals

import io.dac.mara.core.MaraValue
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/2/16.
  */
trait EvalLiteral extends EvalOp with LiteralAlg[Eval] {
  import MaraValue.implicits._

  override def litInt(it: Int): Eval = op { it }
  override def litString(it: String): Eval = op { it }
  override def litbool(it: Boolean): Eval = op { it }
}
