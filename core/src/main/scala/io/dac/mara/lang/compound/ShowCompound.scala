package io.dac.mara.lang.compound

import io.dac.mara.phases.{Show, ShowOp}

/**
  * Created by dcollins on 3/24/17.
  */
trait ShowCompound extends ShowOp with CompoundAlg[Show] {

  override def module(exprs: Seq[Show]): Show = op {
    exprs.map(_.show).mkString("\n")
  }

  override def dox(exprs: Seq[Show]) =
    op { s"do {${exprs.map(_.show).mkString("; ")}}" }
}
