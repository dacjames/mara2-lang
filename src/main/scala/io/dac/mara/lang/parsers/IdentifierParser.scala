package io.dac.mara.lang.parsers

import org.parboiled2.{CharPredicate, Parser}

/**
  * Created by dcollins on 8/13/16.
  */
trait IdentifierParser extends Parser with WhitespaceParser {
  def ValueId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.AlphaNum)) ~ Whitespace
  }

  def TypeId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum)) ~ Whitespace
  }

}
