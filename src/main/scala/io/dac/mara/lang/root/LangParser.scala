package io.dac.mara.lang.root

import io.dac.mara.core.Expr
import io.dac.mara.lang.parsers.{BlockParser, ExprParser, WhitespaceParser}
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E, Alg <: LangAlg[E]] extends Parser
  with WhitespaceParser with ExprParser[E, Alg] with BlockParser[E, Alg] {

  def input: ParserInput
  def alg: Alg


  def Root: Rule1[E]
  def Terminal: Rule1[E]


  def Do: Rule1[E] = rule {
    "do" ~ Block ~> { (x: Seq[E]) =>
      alg.block(x)
    }
  }

  def Empty: Rule1[E] = rule {
    MATCH ~> { () => alg.empty }
  }

}