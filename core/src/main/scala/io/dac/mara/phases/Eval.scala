package io.dac.mara.phases

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */


trait Eval extends Expr[Eval] {
  override type Target = MaraValue
  override def value: MaraValue = eval

  def eval: MaraValue
}

object Eval {
  implicit object EvalPhase extends Phase[Eval] {
    override def key: Int = 2
  }
}


trait EvalOp extends ExprOps[Eval] {
  override def opimpl(f: => MaraValue, index: TreeIndex): Eval =
    context.put(index)(new Eval {
      override def eval = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
}


