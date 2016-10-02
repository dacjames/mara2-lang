package io.dac.mara.lang.operators

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait OperatorParser[E <: Expr, Alg <: OperatorAlg[E]] extends LangParser[E, Alg] {
  def Operator: Rule1[Alg => E] = rule {
    BoolOp
  }

  private[this] def BoolOp = rule {
    ArgLow ~ zeroOrMore(
      '@' ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.base(x(alg), y(alg))} |
      '<' ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.lt(x(alg), y(alg))} |
      '>' ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.gt(x(alg), y(alg))} |
      "<=" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.lte(x(alg), y(alg))} |
      ">=" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.gte(x(alg), y(alg))} |
      "&&" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.and(x(alg), y(alg))} |
      "||" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.or(x(alg), y(alg))} |
      "~&" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.nand(x(alg), y(alg))} |
      "==" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.eq(x(alg), y(alg))} |
      "!=" ~ ArgLow ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.ne(x(alg), y(alg))}
    )
  }

  private[this] def ArgLow = rule {
    ArgMedium ~ zeroOrMore(
      '$' ~ ArgMedium ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.low(x(alg), y(alg)) } |
      '+' ~ ArgMedium ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.plus(x(alg), y(alg)) } |
      '-' ~ ArgMedium ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.minus(x(alg), y(alg)) }
    )
  }

  private[this] def ArgMedium = rule {
    ArgHigh ~ zeroOrMore(
      '%' ~ ArgHigh ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.medium(x(alg), y(alg))} |
      '*' ~ ArgHigh ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.times(x(alg), y(alg))} |
      '/' ~ ArgHigh ~> { (x: Alg => E, y: Alg => E) => (alg: Alg) => alg.divide(x(alg), y(alg))}
    )
  }

  private[this] def ArgHigh = rule {
    "~" ~ Terminal ~> { (x: Alg => E) => (alg: Alg) => alg.not(x(alg)) } |
      Terminal ~ zeroOrMore(
      '^'  ~ Terminal ~> {(x: Alg => E, y: Alg => E) => (alg: Alg) => alg.high(x(alg), y(alg))} |
      "**" ~ Terminal ~> {(x: Alg => E, y: Alg => E) => (alg: Alg) => alg.power(x(alg), y(alg))}
    )
  }


}
