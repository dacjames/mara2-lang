package io.dac.mara.lang.operators

import io.dac.mara.core._
import io.dac.mara.phases.{Compiled, CompiledOp, Typed}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledOperator extends CompiledOp with OperatorAlg[Compiled] with ModuleLookup {
  import io.dac.mara.ir.IrModel._

  private[this] def binop(op: String)(x: Compiled, y: Compiled) =
    opWith[Typed] { typex =>
      val outputType = MaraType.lower(typex).get

      val result = nextTemp()
      val xf = x.fragment
      val yf = y.fragment

      val bytecode = xf ++ yf :+
        stmt(l(result), r(s"${op} ${outputType} ${xf.result}, ${yf.result}"))

      bytecode
    }

  override def plus(x: Compiled, y: Compiled): Compiled =
    binop("add")(x, y)

  override def minus(x: Compiled, y: Compiled): Compiled =
    binop("sub")(x, y)
}
