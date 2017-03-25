package io.dac.mara.lang.parsers

import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait WhitespaceParser extends Parser {
  implicit def whitespaceAroundString(s: String): Rule0 = rule {
    Whitespace ~ str(s) ~ Whitespace
  }

  implicit def whitespaceAroundChar(c: Char): Rule0 = rule {
    Whitespace ~ ch(c) ~ Whitespace
  }

  def Whitespace: Rule0 = rule { zeroOrMore(anyOf(" \t")) }
}
