package io.dac.mara.lang.parsers

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangAlg
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait BlockParser[E <: Expr, Alg <: LangAlg[E]] extends Parser with ExprParser[E, Alg] with WhitespaceParser {
  def Block: Rule1[Alg => Seq[E]] = rule {
    "{" ~ Expr ~ zeroOrMore(";" ~ Expr) ~ "}" ~> {
      (a: Alg => E, b: Seq[Alg => E]) => (alg: Alg) =>
        (a +: b).map{ thunk => thunk(alg) }
    }
  }
}
