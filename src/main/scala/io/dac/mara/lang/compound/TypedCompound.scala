package io.dac.mara.lang.compound

import io.dac.mara.core.MaraType
import io.dac.mara.core.MaraType.EmptyType
import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 3/24/17.
  */
trait TypedCompound extends TypedOp with CompoundAlg[Typed] {
  override def empty = op { EmptyType() }

  private[this] val blockPartial =
    new PartialFunction[Typed, MaraType] {
      private[this] var it: MaraType = _
      override def isDefinedAt(x: Typed) = { it = x.typex; ! it.isInstanceOf[EmptyType]}
      override def apply(v1: Typed) = it
    }

  override def dox(exprs: Seq[Typed]): Typed = op {
    exprs.collect(blockPartial).last
  }

}
