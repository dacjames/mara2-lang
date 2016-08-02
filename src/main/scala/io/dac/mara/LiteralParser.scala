package io.dac.mara

import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralParser[E, T <: LiteralAlg[E]] extends Parser {
  def alg: T
  def input: ParserInput

  def LiteralExpr = rule { StringLiteral | IntLiteral }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def quotedStringLiteral(quote: Char) = rule {
    quote  ~
      capture(zeroOrMore(noneOf(quote.toString))) ~> { x => alg.litString(x) } ~
    quote
  }

  def StringLiteral = rule { quotedStringLiteral('\'') | quotedStringLiteral('\"')}

  def IntLiteral = rule {
    capture(Digits) ~> { x => alg.litInt(x.toInt) }
  }
}
