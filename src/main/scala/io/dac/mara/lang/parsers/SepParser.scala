package io.dac.mara.lang.parsers

import org.parboiled2.Parser

/**
  * Created by dcollins on 10/8/16.
  */
trait SepParser extends Parser with WhitespaceParser {
  def ExprSep = rule {
    ";" | "\n"
  }

  def ListSep = rule {
    ";" | "\n"
  }
}
