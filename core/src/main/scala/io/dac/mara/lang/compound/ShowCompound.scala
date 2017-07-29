package io.dac.mara.lang.compound

import io.dac.mara.phases.{Show, ShowOp}

/**
  * Created by dcollins on 3/24/17.
  */
trait ShowCompound extends ShowOp with CompoundAlg[Show] {
  override def empty = op { "" }

  override def dox(exprs: Seq[Show]) =
    op { s"do {${exprs.map(_.show).mkString("; ")}}" }
}
