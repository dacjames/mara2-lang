package io.dac.mara.lang.variables

import io.dac.mara.core.{Expr, ExprAlg}

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableAlg[E <: Expr] extends ExprAlg[E] {
  def valdeclare(name: String, typex: Option[String]): E = ???
  def valassign(name: String, typex: Option[String], value: E): E = ???
  def valsubstitution(name: String): E = ???
  def typesubstitution(name: String): E = ???
  def block(exprs: Seq[E]): E = ???
}
