package io.dac.mara.lang.parsers

import org.parboiled2.{CharPredicate, Parser, Rule1}

/**
  * Created by dcollins on 8/13/16.
  */
trait IdentifierParser extends Parser with WhitespaceParser {


  def ValueId: Rule1[String] = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }

  def TypeId: Rule1[String] = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }

}
