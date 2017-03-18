package io.dac.mara.lang.parsers

import io.dac.mara.core
import io.dac.mara.lang.root.LangAlg
import org.parboiled2.{Parser, _}

/**
  * Created by dcollins on 10/1/16.
  */
trait ParensParser[E <: core.Expr, Alg <: LangAlg[E]] extends Parser with ExprParser[E, Alg] {
  def Parens: Rule1[E] = rule { '(' ~ Expr ~ ')' }
}
