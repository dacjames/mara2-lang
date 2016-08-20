package io.dac.mara.exprops

import io.dac.mara.core.Expr
import io.dac.mara.core.Expr.Family

/**
  * Created by dcollins on 8/2/16.
  */
case class Show(show: String) extends Expr

object Show {
  implicit object ShowFamily extends Family[Show, String] {
    override def value(e: Show) = e.show
  }
}

trait ShowOp {
  def op(f: => String) = Show(f)
}
