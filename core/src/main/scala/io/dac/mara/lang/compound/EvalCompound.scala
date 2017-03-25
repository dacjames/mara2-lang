package io.dac.mara.lang.compound

import io.dac.mara.core.MaraValue.EmptyValue
import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.core.MaraValue

/**
  * Created by dcollins on 3/24/17.
  */
trait EvalCompound extends EvalOp with CompoundAlg[Eval] {
  import MaraValue._

  override def empty = op { EmptyValue() }

  private[this] val blockPartial =
    new PartialFunction[Eval, MaraValue] {
      private[this] var it: MaraValue = _
      override def isDefinedAt(x: Eval) = { it = x.eval; ! it.isInstanceOf[EmptyValue]}
      override def apply(v1: Eval) = it
    }

  override def dox(block: Seq[Eval]) =
    op {
      block.collect(blockPartial).last
    }
}
