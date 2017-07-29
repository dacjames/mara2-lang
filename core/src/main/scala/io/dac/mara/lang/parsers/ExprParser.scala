package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait ExprParser[E, Alg <: ExprAlg[E]] extends Parser {
  def Expr: Rule1[E]
}
