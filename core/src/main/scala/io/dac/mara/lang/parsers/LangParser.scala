package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E, Alg <: ExprAlg[E]] extends Parser
  with WhitespaceParser with ExprParser[E, Alg] {

  def input: ParserInput
  def alg: Alg

  def Root: Rule1[E]
  def Terminal: Rule1[E]
}