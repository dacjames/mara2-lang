package io.dac.mara.lang.literals

import io.dac.mara.exprops.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledLiteral extends LiteralAlg[Compiled] with CompiledOp {
  override def litint(it: Int): Compiled =
    op { (Seq.empty, it.toString) }
}
