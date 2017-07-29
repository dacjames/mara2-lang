package io.dac.mara.lang.literals

import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralAlg[+E] extends LangAlg[E] {
  def litint(it: Int): E = ???
  def litstring(it: String): E = ???
  def litbool(it: Boolean): E = ???
}

