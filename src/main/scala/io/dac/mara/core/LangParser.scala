package io.dac.mara.core

import io.dac.mara.lang.literals.LiteralAlg
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E <: Expr, Alg <: ExprAlg[E]] extends Parser {
  def alg: Alg
  def input: ParserInput

  def Root: Rule1[E]
  def Expr: Rule1[E]
  def Terminal: Rule1[E]

  def Whitespace = rule {
    zeroOrMore(anyOf(" \t"))
  }

  implicit def whitespaceAfterString(s: String): Rule0 = rule {
    str(s) ~ Whitespace
  }

  implicit def whitespaceAfterChar(c: Char): Rule0 = rule {
    ch(c) ~ Whitespace
  }
}