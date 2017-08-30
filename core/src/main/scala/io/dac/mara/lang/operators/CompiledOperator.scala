package io.dac.mara.lang.operators

import io.dac.mara.core._
import io.dac.mara.phases.{Compiled, CompiledOp, Typed}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledOperator extends CompiledOp with OperatorAlg[Compiled] with ModuleLookup {
  import io.dac.mara.ir.implicits._

  override def plus(x: Compiled, y: Compiled): Compiled =
    op {
      val outputIsFirstArg = MaraType.lower(x.get[Typed]).get

      val result = nextTemp()
      val bytecode =
        x.bytecode ++
        y.bytecode +|
        s"$result = add ${outputIsFirstArg} ${x.result}, ${y.result}"

      (bytecode, result)
    }

  override def minus(x: Compiled, y: Compiled): Compiled =
    op {
      val outputIsFirstArg = MaraType.lower(x.get[Typed]).get

      val result = nextTemp()
      val bytecode =
        x.bytecode ++
          y.bytecode +|
          s"$result = sub ${outputIsFirstArg} ${x.result}, ${y.result}"

      (bytecode, result)
    }
}
