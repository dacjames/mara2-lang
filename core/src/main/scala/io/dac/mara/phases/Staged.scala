package io.dac.mara.phases

import io.dac.mara.core.MaraValue.UnitValue
import io.dac.mara.core._

trait Staged extends Expr[Staged] {
  override type Target = MaraValue

  override def value: MaraValue = stage
  def stage: Target

}

object Staged {
  implicit object StagePhase extends Phase[Staged] {
    override def key: Int = 5
  }

  implicit object StagedEmpty extends Empty[Staged] {
    override def empty: Staged =
      new Staged {
        override def stage: MaraValue = UnitValue()
        override def get[A <: Expr[A] : Phase]: A#Target = ???
      }
  }

  def empty: Staged#Target = MaraValue.UnitValue()

}

trait StagedOp extends ExprOps[Staged] {
  def opimpl(f: => MaraValue, index: TreeIndex): Staged = {
    context.put(index)(new Staged {
      override def stage: Target = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
  }
}