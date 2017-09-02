package io.dac.mara.lang.variables

import io.dac.mara.core._

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableAlg[E] extends ExprAlg[E] {
  def valdeclare(name: String, typex: Option[String]): E = empty
  def valassign(name: String, typex: Option[String], value: E): E = empty
  def valsubstitution(name: String): E = empty
  def typesubstitution(name: String): E = empty
}
