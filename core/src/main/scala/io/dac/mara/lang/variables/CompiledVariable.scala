package io.dac.mara.lang.variables

import io.dac.mara.phases.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledVariable extends CompiledOp with VariableAlg[Compiled] {
  import io.dac.mara.ir.IrModel._

  override def valsubstitution(name: String): Compiled =
    op { Fragment(l(s"%$name")) }


}
