package io.dac.mara.core

import io.dac.mara.literals.LiteralAlg
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E <: Expr, T <: ExprAlg[E]] extends Parser {
  def alg: T
  def input: ParserInput

  def Root: Rule1[E]
  def Expr: Rule1[E]
  def Terminal: Rule1[E]
  def Whitespace = rule { anyOf(" \t") }
}
