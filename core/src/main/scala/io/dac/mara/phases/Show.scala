package io.dac.mara.phases

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */
trait Show extends Expr[Show] {
  override type Target = String
  override def value: String = show

  def show: String
}

object Show {
  implicit object ShowPhase extends Phase[Show] {
    override def key: Int = 0
  }
}

trait ShowOp extends ExprOps[Show] {
  override def opimpl(f: => String, index: TreeIndex): Show =
    context.put(index)(new Show {
      override def show = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
}
