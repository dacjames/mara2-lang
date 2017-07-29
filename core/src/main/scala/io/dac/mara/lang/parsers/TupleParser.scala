package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait TupleParser[E, Alg <: ExprAlg[E]] extends Parser with ExprParser[E, Alg] with WhitespaceParser {
  def TupleSyntax: Rule1[Seq[E]] = rule {
    '(' ~ Expr ~ zeroOrMore(',' ~ Expr) ~ optional(',') ~ ')' ~> { (a: E, b: Seq[E]) =>
      a +: b
    }
  }
}

