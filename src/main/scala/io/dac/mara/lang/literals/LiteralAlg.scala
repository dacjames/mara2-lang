package io.dac.mara.lang.literals

import io.dac.mara.core.{Expr, ExprAlg}

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralAlg[E <: Expr] extends ExprAlg[E] {
  def litInt(it: Int): E
  def litString(it: String): E
}

