package io.dac.mara.lang.literals

import io.dac.mara.exprops._

/**
  * Created by dcollins on 3/18/17.
  */
class PipelineLiteral(show: LiteralAlg[Show], typed: LiteralAlg[Typed], eval: LiteralAlg[Eval])
  extends  LiteralAlg[(Show, Typed, Eval)] {

  override def litint(it: Int): (Show, Typed, Eval) =
    (show.litint(it), typed.litint(it), eval.litint(it))

  override def litstring(it: String): (Show, Typed, Eval) =
    (show.litstring(it), typed.litstring(it), eval.litstring(it))

  override def litbool(it: Boolean): (Show, Typed, Eval) =
    (show.litbool(it), typed.litbool(it), eval.litbool(it))
}
