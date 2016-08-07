package io.dac.mara.literals

import io.dac.mara.core.ExprAlg

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralAlg[E] extends ExprAlg[E] {
  def litInt(it: Int): E
  def litString(it: String): E
}

