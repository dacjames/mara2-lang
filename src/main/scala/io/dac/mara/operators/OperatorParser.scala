package io.dac.mara.operators

import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorParser[E, T <: OperatorAlg[E]] extends Parser {
  def alg: T
  def input: ParserInput

  def Expr: Rule1[E]
  def Terminal: Rule1[E]

  def BinOp = rule {
    ArgLow ~ zeroOrMore(
      '<' ~ ArgLow ~> { (x: E, y) => alg.lt(x, y)} |
      '>' ~ ArgLow ~> { (x: E, y) => alg.gt(x, y)} |
      "<=" ~ ArgLow ~> { (x: E, y) => alg.lte(x, y)} |
      ">=" ~ ArgLow ~> { (x: E, y) => alg.gte(x, y)} |
      "&&" ~ ArgLow ~> { (x: E, y) => alg.and(x, y)} |
      "||" ~ ArgLow ~> { (x: E, y) => alg.or(x, y)})
  }

  private[this] def ArgLow = rule {
    ArgMedium ~ zeroOrMore(
      '+' ~ ArgMedium ~> { (x: E, y) => alg.plus(x, y) } |
      '-' ~ ArgMedium ~> { (x: E, y) => alg.minus(x, y) }
    )
  }

  private[this] def ArgMedium: Rule1[E] = rule {
    ArgHigh ~ zeroOrMore(
      '*' ~ ArgHigh ~> { (x: E, y) => alg.times(x, y)} |
      '/' ~ ArgHigh ~> { (x: E, y) => alg.divide(x, y)}
    )
  }

  private[this] def ArgHigh: Rule1[E] = rule {
    Terminal ~ zeroOrMore(
      '^' ~ Terminal ~> {(x: E, y) => alg.power(x, y)}
    )
  }


}
