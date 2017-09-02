package io.dac.mara.phases

import io.dac.mara.core.MaraType.UnitType
import io.dac.mara.core._

/**
  * Created by dcollins on 8/27/16.
  */


trait Typed extends Expr[Typed] {
  override type Target = MaraType
  override def value: MaraType = typex

  def typex: MaraType
}

object Typed {
  implicit object TypedPhase extends Phase[Typed] {
    override def key: Int = 1
  }

  implicit object TypedEmpty extends Empty[Typed] {
    override def empty: Typed =
      new Typed {
        override def typex: MaraType = UnitType()
        override def get[A <: Expr[A] : Phase]: A#Target = ???
      }
  }
}

trait TypedOp extends ExprOps[Typed] {
  override def opimpl(f: => MaraType, index: TreeIndex): Typed = {
    context.put[Typed](index)(new Typed {
      override def typex = f
      override def get[E <: Expr[E] : Phase]: E#Target = context.get[E](index)
    })
  }
}

