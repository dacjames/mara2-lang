package io.dac.mara.lang.parsers

import io.dac.mara.core.{Expr, ExprAlg}
import org.parboiled2._

/**
  * Created by dcollins on 8/6/16.
  */
trait LangParser[E <: Expr, Alg <: ExprAlg[E]] extends Parser with WhitespaceParser {
  def alg: Alg
  def input: ParserInput

  def Root: Rule1[E]
  def Expr: Rule1[E]
  def Terminal: Rule1[E]


  implicit def whitespaceAfterString(s: String): Rule0 = rule {
    str(s) ~ Whitespace
  }

  implicit def whitespaceAfterChar(c: Char): Rule0 = rule {
    ch(c) ~ Whitespace
  }
}