package io.dac.mara.variables

import io.dac.mara.core.{Expr, ExprAlg}

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableAlg[E <: Expr] extends ExprAlg[E] {
  def valdeclare(name: String, typex: Option[String]): E = ???
  def valassign(name: String, typex: Option[String], value: E): E = ???
  def substitution(name: String): E = ???
  def block(e1: E, e2: E): E = ???
}
