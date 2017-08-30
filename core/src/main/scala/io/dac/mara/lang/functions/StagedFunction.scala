package io.dac.mara.lang.functions

import io.dac.mara.core._
import io.dac.mara.phases.{Staged, StagedOp}

trait StagedFunction extends StagedOp with FunctionAlg[Staged]{

  override def funconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[Staged]): Staged = op {
    Staged.empty
  }

  override def call(name: String, args: Seq[Staged]): Staged = op {
    Staged.empty
  }
}
