package io.dac.mara.lang.compound

import io.dac.mara.core.MaraType
import io.dac.mara.core.MaraType.EmptyType
import io.dac.mara.exprops.{Show, ShowOp, Typed}

/**
  * Created by dcollins on 3/24/17.
  */
trait ShowCompound extends ShowOp with CompoundAlg[Show] {
  override def empty = op { "" }

  override def dox(exprs: Seq[Show]) =
    op { s"{${exprs.map(_.show).mkString("; ")}}" }
}
