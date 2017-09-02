package io.dac.mara.lang.literals

import io.dac.mara.phases.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledLiteral extends CompiledOp with LiteralAlg[Compiled] {
  import io.dac.mara.ir.IrModel._

  override def litint(it: Int): Compiled =
    op { Fragment(l(it.toString)) }
}
