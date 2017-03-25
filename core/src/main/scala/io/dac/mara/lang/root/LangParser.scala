package io.dac.mara.lang.root

import io.dac.mara.lang.parsers.{ExprParser, WhitespaceParser}
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E, Alg <: LangAlg[E]] extends Parser
  with WhitespaceParser with ExprParser[E, Alg] {

  def input: ParserInput
  def alg: Alg

  def Root: Rule1[E]
  def Terminal: Rule1[E]
}