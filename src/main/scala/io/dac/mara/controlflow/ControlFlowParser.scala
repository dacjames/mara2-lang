package io.dac.mara.controlflow

import io.dac.mara.core.LangParser
import io.dac.mara.literals.LiteralAlg
import org.parboiled2.{Parser, ParserInput}

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowParser[E, T <: ControlFlowAlg[E]] extends LangParser[E, T] {
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
