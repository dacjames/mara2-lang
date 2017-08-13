package io.dac.mara.phases

import io.dac.mara.core.{Expr, ExprOps, Phase}
import io.dac.mara.ir.IrFragment

import scala.collection.mutable

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

  def empty: Compiled#Target = (Vector.empty[IrFragment], new IrFragment("0"))

  def recurse(block: Seq[Compiled]) = {
    val bytecode = mutable.ArrayBuffer.empty[IrFragment]
    var result: Option[IrFragment] = None

    block.foreach { compiled =>
      bytecode ++= compiled.bytecode
      result = Some(compiled.result)
    }


    result match {
      case Some(result) => (bytecode.toVector, result)
      case None => Compiled.empty
    }
  }
}

trait CompiledOp extends ExprOps[Compiled] {
  override def op(f: => (Vector[IrFragment], IrFragment)): Compiled =
    new Compiled {
      private[this] lazy val capture = f
      override def bytecode = capture._1
      override def result = capture._2
      override val phase = context.nextIndex[Compiled]
    }
}
