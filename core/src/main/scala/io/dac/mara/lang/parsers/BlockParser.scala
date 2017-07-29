package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait BlockParser[E, Alg <: ExprAlg[E]] extends Parser with ExprParser[E, Alg] with WhitespaceParser with SepParser {
  def Block: Rule1[Seq[E]] = rule {
    "{" ~ Expr ~ zeroOrMore(ExprSep ~ Expr) ~ "}" ~> {
      (a: E, b: Seq[E]) => a +: b
    }
  }

}
