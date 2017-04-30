package io.dac.mara.lang.variables

import io.dac.mara.exprops.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledVariable extends VariableAlg[Compiled] with CompiledOp {
  import io.dac.mara.ir.implicits._

  override def valsubstitution(name: String): Compiled =
    op { (Vector.empty, s"%$name") }


}
