package io.dac.mara.lang.variables

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableAlg[E <: Expr] extends LangAlg[E] {
  def valdeclare(name: String, typex: Option[String]): E = ???
  def valassign(name: String, typex: Option[String], value: E): E = ???
  def valsubstitution(name: String): E = ???
  def typesubstitution(name: String): E = ???
}
