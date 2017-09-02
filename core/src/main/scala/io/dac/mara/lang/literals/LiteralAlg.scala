package io.dac.mara.lang.literals

import io.dac.mara.core._

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralAlg[E] extends ExprAlg[E] {
  def litint(it: Int): E = empty
  def litstring(it: String): E = empty
  def litbool(it: Boolean): E = empty
}

