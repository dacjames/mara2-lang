package io.dac.mara.lang.parsers

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangAlg
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait TupleParser[E, Alg <: LangAlg[E]] extends Parser with ExprParser[E, Alg] with WhitespaceParser {
  def Tuple: Rule1[Seq[E]] = rule {
    '(' ~ Expr ~ zeroOrMore(',' ~ Expr) ~ ')' ~> { (a: E, b: Seq[E]) =>
      a +: b
    }
  }
}

