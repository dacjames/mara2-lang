package io.dac.mara.lang.variables

import io.dac.mara.exprops.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledVariable extends VariableAlg[Compiled] with CompiledOp{
  override def valsubstitution(name: String): Compiled =
    op { (Seq.empty, s"%$name") }
}
