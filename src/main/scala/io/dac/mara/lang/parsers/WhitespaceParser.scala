package io.dac.mara.lang.parsers

import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait WhitespaceParser extends Parser {
  implicit def whitespaceAfterString(s: String): Rule0 = rule {
    str(s) ~ Whitespace
  }

  implicit def whitespaceAfterChar(c: Char): Rule0 = rule {
    ch(c) ~ Whitespace
  }

  def Whitespace: Rule0 = rule { zeroOrMore(anyOf(" \t")) }
}
