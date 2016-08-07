package io.dac.mara.operators

import io.dac.mara.core.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorParser[E, T <: OperatorAlg[E]] extends LangParser[E, T] {
  def Operator = rule {
    BoolOp
  }

  private[this] def BoolOp = rule {
    ArgLow ~ zeroOrMore(
      '<' ~ ArgLow ~> { (x: E, y) => alg.lt(x, y)} |
      '>' ~ ArgLow ~> { (x: E, y) => alg.gt(x, y)} |
      "<=" ~ ArgLow ~> { (x: E, y) => alg.lte(x, y)} |
      ">=" ~ ArgLow ~> { (x: E, y) => alg.gte(x, y)} |
      "&&" ~ ArgLow ~> { (x: E, y) => alg.and(x, y)} |
      "||" ~ ArgLow ~> { (x: E, y) => alg.or(x, y)}
    )
  }

  private[this] def ArgLow = rule {
    ArgMedium ~ zeroOrMore(
      '+' ~ ArgMedium ~> { (x: E, y) => alg.plus(x, y) } |
      '-' ~ ArgMedium ~> { (x: E, y) => alg.minus(x, y) }
    )
  }

  private[this] def ArgMedium = rule {
    ArgHigh ~ zeroOrMore(
      '*' ~ ArgHigh ~> { (x: E, y) => alg.times(x, y)} |
      '/' ~ ArgHigh ~> { (x: E, y) => alg.divide(x, y)}
    )
  }

  private[this] def ArgHigh = rule {
    "~" ~ Terminal ~> { (x: E) => alg.not(x) } |
    Terminal ~ zeroOrMore(
      '^' ~ Terminal ~> {(x: E, y) => alg.power(x, y)}
    )
  }


}
