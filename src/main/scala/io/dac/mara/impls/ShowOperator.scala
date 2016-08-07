package io.dac.mara.impls

import io.dac.mara.exprops.{Show, ShowOp}
import io.dac.mara.operators.OperatorAlg

/**
  * Created by dcollins on 8/2/16.
  */
trait ShowOperator extends ShowOp with OperatorAlg[Show] {
  private[this] def binop(x: Show, y: Show, sym: String) = op { s"(${x.show} + ${y.show})" }

  override def plus(x: Show, y: Show) = binop(x, y, "+")
  override def minus(x: Show, y: Show) = binop(x, y, "-")
  override def times(x: Show, y: Show) = binop(x, y, "*")
  override def divide(x: Show, y: Show) = binop(x, y, "/")
  override def power(x: Show, y: Show) = binop(x, y, "^")

  override def lt(x: Show, y: Show) = binop(x, y, "<")
  override def gt(x: Show, y: Show) = binop(x, y, ">")
  override def lte(x: Show, y: Show) = binop(x, y, "<=")
  override def gte(x: Show, y: Show) = binop(x, y, ">=")
  override def and(x: Show, y: Show) = binop(x, y, "&&")
  override def or(x: Show, y: Show) = binop(x, y, "||")
}
