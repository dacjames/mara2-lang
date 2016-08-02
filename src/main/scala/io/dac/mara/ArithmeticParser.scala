package io.dac.mara

import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait ArithmeticParser[E, T <: ArithmeticAlg[E]] extends Parser {
  def alg: T
  def input: ParserInput

  def Expr: Rule1[E]
  def TerminalExpr: Rule1[E]

  def ArithmeticExpr: Rule1[E] = rule {
    Term ~ zeroOrMore(
          '+' ~ Term ~> { (x: E, y) => alg.plus(x, y) }
        | '-' ~ Term ~> { (x: E, y) => alg.minus(x, y) }
    )
  }

  def Term: Rule1[E] = rule {
    Factor ~ zeroOrMore(
          '*' ~ Factor ~> {(x: E, y) => alg.times(x, y)}
        | '/' ~ Factor ~> {(x: E, y) => alg.divide(x, y)}
    )
  }

  def Factor: Rule1[E] = rule {
    SubFactor ~ zeroOrMore(
      '^' ~ SubFactor ~> {(x: E, y) => alg.power(x, y)}
    )
  }

  def SubFactor = rule { TerminalExpr | Parens }

  def Parens = rule { '(' ~ Expr ~ ')' }

}
