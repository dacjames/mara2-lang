package io.dac.mara.lang.parsers

import org.parboiled2.Parser

/**
  * Created by dcollins on 8/28/16.
  */
trait WhitespaceParser extends Parser {
  def Whitespace = rule { zeroOrMore(anyOf(" \t")) }
}
