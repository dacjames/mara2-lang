package io.dac.mara.lang.parsers

import io.dac.mara.core.Expr
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait BlockParser[E <: Expr] extends Parser with ExprParser[E] with WhitespaceParser {
  def Block: Rule1[Seq[E]] = rule {
    '{' ~ Expr ~ zeroOrMore(';' ~ Expr) ~ '}' ~> { (a: E, b: Seq[E]) => a +: b }
  }
}
