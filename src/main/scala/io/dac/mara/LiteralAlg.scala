package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralAlg[E] extends ExprAlg[E] {
  def litInt(it: Int): E
  def litString(it: String): E
}

