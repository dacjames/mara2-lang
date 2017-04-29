package io.dac.mara.lang.operators

import io.dac.mara.exprops.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledOperator extends OperatorAlg[Compiled] with CompiledOp {
  override def plus(x: Compiled, y: Compiled): Compiled =
    op {
      val bytecode =
        x.bytecode ++ y.bytecode ++ Seq(
          s"%t2 = add i32 ${x.result}, ${y.result};"
        )
      val result = "%t2"

      (bytecode, result)
    }
}
