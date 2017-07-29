package io.dac.mara.lang.operators

import io.dac.mara.phases.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledOperator extends CompiledOp with OperatorAlg[Compiled]  {
  import io.dac.mara.ir.implicits._

  override def plus(x: Compiled, y: Compiled): Compiled =
    op {
      val bytecode =
        x.bytecode ++
        y.bytecode +|
        s"%t2 = add i32 ${x.result}, ${y.result};"

      val result = "%t2"

      (bytecode, result)
    }
}
