package io.dac.mara.lang.literals

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangParser
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralParser[E <: Expr, Alg <: LiteralAlg[E]] extends LangParser[E, Alg] {
  def Literal: Rule1[Alg => E] = rule { ActualLiteral ~ Whitespace }

  private[this] def ActualLiteral = rule { StringLiteral | IntLiteral | BoolLiteral }

  private[this] def Digits = rule { oneOrMore(CharPredicate.Digit) }

  private[this] def quoted(quote: Char) = rule {
    quote  ~
      capture(zeroOrMore(noneOf(quote.toString))) ~> { x => (alg: Alg) => alg.litstring(x) } ~
    quote
  }

  private[this] def StringLiteral = rule { quoted('\'') | quoted('\"')}

  private[this] def IntLiteral = rule {
    capture(Digits) ~> { x => (alg: Alg) => alg.litint(x.toInt) }
  }

  private[this] def BoolLiteral = rule {
    capture("true" | "false") ~> {  x: String => (alg: Alg) =>
      if (x.trim == "true") { alg.litbool(true) }
      else { alg.litbool(false) }
    }
  }
}