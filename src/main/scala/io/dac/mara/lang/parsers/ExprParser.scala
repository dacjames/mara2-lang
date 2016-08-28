package io.dac.mara.lang.parsers

import io.dac.mara.core.Expr
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait ExprParser[E <: Expr] extends Parser {
  def Expr: Rule1[E]
}
