package io.dac.mara.lang.operators

import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorParser[E, Alg <: OperatorAlg[E]] extends LangParser[E, Alg] {
  def Operator: Rule1[E] = rule {
    BoolOp
  }

  private[this] def BoolOp = rule {
    ArgLow ~ zeroOrMore(
      '@' ~ ArgLow ~> { (x: E, y: E) => alg.base(x, y)} |
      '<' ~ ArgLow ~> { (x: E, y: E) => alg.lt(x, y)} |
      '>' ~ ArgLow ~> { (x: E, y: E) => alg.gt(x, y)} |
      "<=" ~ ArgLow ~> { (x: E, y: E) => alg.lte(x, y)} |
      ">=" ~ ArgLow ~> { (x: E, y: E) => alg.gte(x, y)} |
      "&&" ~ ArgLow ~> { (x: E, y: E) => alg.and(x, y)} |
      "||" ~ ArgLow ~> { (x: E, y: E) => alg.or(x, y)} |
      "~&" ~ ArgLow ~> { (x: E, y: E) => alg.nand(x, y)} |
      "==" ~ ArgLow ~> { (x: E, y: E) => alg.eq(x, y)} |
      "!=" ~ ArgLow ~> { (x: E, y: E) => alg.ne(x, y)}
    )
  }

  private[this] def ArgLow = rule {
    ArgMedium ~ zeroOrMore(
      '$' ~ ArgMedium ~> { (x: E, y: E) => alg.low(x, y) } |
      '+' ~ ArgMedium ~> { (x: E, y: E) => alg.plus(x, y) } |
      '-' ~ ArgMedium ~> { (x: E, y: E) => alg.minus(x, y) }
    )
  }

  private[this] def ArgMedium = rule {
    ArgHigh ~ zeroOrMore(
      '%' ~ ArgHigh ~> { (x: E, y: E) => alg.medium(x, y)} |
      '*' ~ ArgHigh ~> { (x: E, y: E) => alg.times(x, y)} |
      '/' ~ ArgHigh ~> { (x: E, y: E) => alg.divide(x, y)}
    )
  }

  private[this] def ArgHigh = rule {
    "~" ~ Terminal ~> { (x: E) => alg.not(x) } |
      Terminal ~ zeroOrMore(
      '^'  ~ Terminal ~> {(x: E, y: E) => alg.high(x, y)} |
      "**" ~ Terminal ~> {(x: E, y: E) => alg.power(x, y)}
    )
  }


}
