package io.dac.mara.exprops

import io.dac.mara.core.Expr

/**
  * Created by dcollins on 8/2/16.
  */
case class Show(show: String) extends Expr

trait ShowOp {
  def op(f: => String) = Show(f)
}
