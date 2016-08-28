package io.dac.mara.lang.controlflow

import io.dac.mara.core.Expr
import io.dac.mara.lang.literals.LiteralAlg
import io.dac.mara.lang.parsers.LangParser
import org.parboiled2.{Parser, ParserInput}

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowParser[E <: Expr, T <: ControlFlowAlg[E]] extends LangParser[E, T] {
  def ControlFlow = rule {
    If
  }

  def If = rule {
    "if" ~ Expr ~ TempBlock ~ Else ~> { (p: E, b: E, o: E) => alg.elsex(alg.ifx(p, b), o)} |
    "if" ~ (Expr ~ TempBlock) ~> { (p: E, b: E) => alg.ifx(p, b) }

  }

  def Else = rule {
     "else" ~ TempBlock
  }

  def TempBlock = rule {
    "{" ~ Expr  ~ "}"
  }
}
