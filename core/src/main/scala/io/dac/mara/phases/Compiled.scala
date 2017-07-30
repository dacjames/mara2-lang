package io.dac.mara.phases

import io.dac.mara.core.{Expr, ExprOps, Phase}
import io.dac.mara.ir.IrFragment

/**
  * Created by dcollins on 4/28/17.
  */
trait Compiled extends Expr[Compiled] {
  override type Target = (Vector[IrFragment], IrFragment)
  override def value: (Vector[IrFragment], IrFragment) = (bytecode, result)

  def bytecode: Vector[IrFragment]
  def result: IrFragment
}

object Compiled {
  implicit object CompiledPhase extends Phase[Compiled] {
    override def key: Int = 3
  }
}

trait CompiledOp extends ExprOps[Compiled] {
  override def op(f: => (Vector[IrFragment], IrFragment)): Compiled =
    new Compiled {
      override def bytecode = f._1
      override def result = f._2
      override val phase = context.nextIndex[Compiled]
    }
}
