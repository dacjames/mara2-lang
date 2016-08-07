package io.dac.mara.literals

import io.dac.mara.core.LangParser
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralParser[E, T <: LiteralAlg[E]] extends LangParser[E, T] {
  def Literal = rule { StringLiteral | IntLiteral }

  private[this] def Digits = rule { oneOrMore(CharPredicate.Digit) }

  private[this] def quoted(quote: Char) = rule {
    quote  ~
      capture(zeroOrMore(noneOf(quote.toString))) ~> { x => alg.litString(x) } ~
    quote
  }

  private[this] def StringLiteral = rule { quoted('\'') | quoted('\"')}

  private[this] def IntLiteral = rule {
    capture(Digits) ~> { x => alg.litInt(x.toInt) }
  }
}
