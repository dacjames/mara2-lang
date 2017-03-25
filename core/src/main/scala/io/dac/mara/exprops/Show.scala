package io.dac.mara.exprops

import io.dac.mara.core.Expr
import io.dac.mara.core.Expr.Replable

/**
  * Created by dcollins on 8/2/16.
  */
trait Show extends Expr {
  def show: String
}

object Show {
  implicit object ShowReplable$ extends Replable[Show, String] {
    override def value(e: Show) = e.show
  }
}

trait ShowOp {
  def op(f: => String) = new Show {
    override def show = f
  }
}
