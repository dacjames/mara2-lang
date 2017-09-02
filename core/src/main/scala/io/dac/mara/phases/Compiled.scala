package io.dac.mara.phases

import io.dac.mara.core._
import io.dac.mara.ir.IrModel
import io.dac.mara.ir.IrModel.{Fragment, Instruction}

import scala.collection.mutable

/**
  * Created by dcollins on 4/28/17.
  */
trait Compiled extends Expr[Compiled] {
  override type Target = IrModel.Fragment
  override def value: IrModel.Fragment = fragment

  def fragment: IrModel.Fragment
}

object Compiled {
  implicit object CompiledPhase extends Phase[Compiled] {
    override def key: Int = 3
  }

  implicit object CompiledEmpty extends Empty[Compiled] {
    override def empty: Compiled =
      new Compiled {
        override def fragment: Fragment = Fragment.empty
        override def get[A <: Expr[A] : Phase]: A#Target = ???
      }
  }

  def recurse(block: Seq[Compiled]): Fragment = {
    block.map(_.fragment).foldLeft(Fragment.empty)(_ ++ _)
  }
}

trait CompiledOp extends ExprOps[Compiled] {
  override def opimpl(f: => Fragment, index: TreeIndex): Compiled = {
    context.put(index)(new Compiled {
      override def fragment: Fragment = f

      override def get[A <: Expr[A] : Phase]: A#Target = context.get[A](index)
    })
  }
}
