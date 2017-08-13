package io.dac.mara.lang.compound

import io.dac.mara.core._
import io.dac.mara.ir.IrFragment
import io.dac.mara.phases.{Compiled, CompiledOp}

import scala.collection.mutable

trait CompiledCompound extends CompiledOp with CompoundAlg[Compiled] {

  override def dox(block: Seq[Compiled]): Compiled = op(Compiled.recurse(block))

}
