package io.dac.mara.lang.compound

import io.dac.mara.core._
import io.dac.mara.phases.{Staged, StagedOp}

trait StagedCompound extends StagedOp with CompoundAlg[Staged] {
  import io.dac.mara.core.MaraValue._
  override def dox(block: Seq[Staged]): Staged = op {
    block.map(_.value).lastOption match {
      case Some(v) => v
      case None => UnitValue()
    }
  }

  override def module(exprs: Seq[Staged]): Staged = this.dox(exprs)
}
