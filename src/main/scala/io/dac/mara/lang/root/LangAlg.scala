package io.dac.mara.lang.root

import io.dac.mara.core.{Expr, ExprAlg}

/**
  * Created by dcollins on 8/28/16.
  */
trait LangAlg[E] {
  def empty: E = ???
  def block(exprs: Seq[E]): E = ???
}
