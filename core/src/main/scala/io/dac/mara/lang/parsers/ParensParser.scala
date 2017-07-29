package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2.{Parser, _}

/**
  * Created by dcollins on 10/1/16.
  */
trait ParensParser[E, Alg <: ExprAlg[E]] extends Parser with ExprParser[E, Alg] {
  def Parens: Rule1[E] = rule { '(' ~ Expr ~ ')' }
}
