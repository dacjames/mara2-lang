package io.dac.mara.exprops

import io.dac.mara.core.Expr.Replable
import io.dac.mara.core.{Expr, ExprOps, Phase, PhaseKey}
import io.dac.mara.ir.IrFragment

/**
  * Created by dcollins on 4/28/17.
  */
trait Compiled extends Expr[Compiled] {
  override type Target = (Vector[IrFragment], IrFragment)
  def bytecode: Vector[IrFragment]
  def result: IrFragment
}

object Compiled {
  implicit object CompileRepl extends Replable[Compiled, String] {
    override def value(e: Compiled) = e.bytecode.mkString("\n")
  }

  implicit object CompiledPhaseKey extends PhaseKey[Compiled] {
    override def key: Int = 3
  }
}

trait CompiledOp extends ExprOps[Compiled] {
  def op(f: => (Vector[IrFragment], IrFragment)): Compiled =
    new Compiled {
      override def bytecode = f._1
      override def result = f._2
      override val phase = context.nextPhase[Compiled]
    }
}
