package io.dac.mara.exprops

import io.dac.mara.core.Expr.Replable
import io.dac.mara.core.Expr
import io.dac.mara.ir.IrFragment

/**
  * Created by dcollins on 4/28/17.
  */
trait Compiled extends Expr {
  def bytecode: Vector[IrFragment]
  def result: IrFragment
}

object Compiled {
  implicit object CompileRepl extends Replable[Compiled, String] {
    override def value(e: Compiled) = e.bytecode.mkString("\n")
  }
}

trait CompiledOp {
  def op(f: => (Vector[IrFragment], IrFragment)): Compiled =
    new Compiled {
      override def bytecode = f._1
      override def result = f._2

    }
}
