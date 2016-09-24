package io.dac.mara.exprops

import io.dac.mara.core.Expr
import io.dac.mara.core.Expr.Family

/**
  * Created by dcollins on 8/2/16.
  */
trait Show extends Expr {
  def show: String
}

object Show {
  implicit object ShowFamily extends Family[Show, String] {
    override def value(e: Show) = e.show
  }
}

trait ShowOp {
  def op(f: => String) = new Show {
    override def show = f
  }
}
