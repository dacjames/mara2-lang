package io.dac.mara.lang.parsers

import org.parboiled2._

/**
  * Created by dcollins on 3/25/17.
  */
trait LiteralSyntaxParser extends Parser {

  private[this] def quoted(quote: Char) = rule {
    quote  ~
      capture(zeroOrMore(noneOf(quote.toString))) ~
    quote
  }

  private[this] def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def StringLiteralSyntax = rule { quoted('\'') | quoted('\"')}

  def IntLiteralSyntax = rule {
    capture(Digits) ~> { x => x.toInt }
  }

}
