package io.dac.mara.lang.identifiers

import org.parboiled2.{CharPredicate, Parser}

/**
  * Created by dcollins on 8/13/16.
  */
trait IdentifierParser extends Parser {
  def ValueId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }

  def TypeId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }
}
