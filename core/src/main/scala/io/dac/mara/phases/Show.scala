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

  implicit object ShowEmpty extends Empty[Show] {
    override def empty: Show =
      new Show {
        override def show: String = ""
        override def get[A <: Expr[A] : Phase]: A#Target = ???
      }
  }
}

trait ShowOp extends ExprOps[Show] {
  override def opimpl(f: => String, index: TreeIndex): Show =
    context.put(index)(new Show {
      override def show = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
}
