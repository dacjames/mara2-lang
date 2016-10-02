package io.dac.mara.lang.operators

import io.dac.mara.exprops.{Show, ShowOp}

trait ShowOperatorSymbols extends ShowOp with OperatorAlg[Show] {
  private[this] def binop(x: Show, y: Show, sym: String) = op { s"(${x.show} ${sym} ${y.show})" }

  override def and(x: Show, y: Show) = binop(x, y, "&&")
  override def or(x: Show, y: Show) = binop(x, y, "||")
  override def not(x: Show)= op { s"~${x.show}" }


}

trait ShowOperatorWords extends ShowOp with OperatorAlg[Show] {
  private[this] def binop(x: Show, y: Show, sym: String) = op { s"(${x.show} ${sym} ${y.show})" }

  override def and(x: Show, y: Show) = binop(x, y, "and")
  override def or(x: Show, y: Show) = binop(x, y, "or")
  override def not(x: Show)= op { s"not ${x.show}" }
}



/**
  * Created by dcollins on 8/2/16.
  */
trait ShowOperator extends ShowOp with ShowOperatorWords with OperatorAlg[Show] {
  private[this] def binop(x: Show, y: Show, sym: String) = op { s"(${x.show} ${sym} ${y.show})" }

  override def plus(x: Show, y: Show) = binop(x, y, "+")
  override def minus(x: Show, y: Show) = binop(x, y, "-")
  override def times(x: Show, y: Show) = binop(x, y, "*")
  override def divide(x: Show, y: Show) = binop(x, y, "/")
  override def power(x: Show, y: Show) = binop(x, y, "**")

  override def lt(x: Show, y: Show) = binop(x, y, "<")
  override def gt(x: Show, y: Show) = binop(x, y, ">")
  override def lte(x: Show, y: Show) = binop(x, y, "<=")
  override def gte(x: Show, y: Show) = binop(x, y, ">=")
  override def ne(x: Show, y: Show) = binop(x, y, "!=")
  override def eq(x: Show, y: Show) = binop(x, y, "==")

  override def base(x: Show, y: Show) = binop(x, y, "@")
  override def low(x: Show, y: Show) = binop(x, y, "$")
  override def medium(x: Show, y: Show) = binop(x, y, "%")
  override def high(x: Show, y: Show) = binop(x, y, "^")
}
