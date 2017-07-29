package io.dac.mara.lang.literals

import io.dac.mara.phases.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledLiteral extends CompiledOp with LiteralAlg[Compiled] {
  import io.dac.mara.ir.implicits._

  override def litint(it: Int): Compiled =
    op { (Vector.empty, it.toString) }
}
