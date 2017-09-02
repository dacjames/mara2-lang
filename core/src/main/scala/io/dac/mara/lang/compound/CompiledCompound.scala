package io.dac.mara.lang.compound

import io.dac.mara.core._

import io.dac.mara.phases.{Compiled, CompiledOp}


trait CompiledCompound extends CompiledOp with CompoundAlg[Compiled] {

  override def module(exprs: Seq[Compiled]): Compiled = this.dox(exprs)

  override def dox(block: Seq[Compiled]): Compiled = op(Compiled.recurse(block))

}
