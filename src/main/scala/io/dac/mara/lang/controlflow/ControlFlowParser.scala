package io.dac.mara.lang.controlflow

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangParser
import org.parboiled2.{Parser, ParserInput, Rule1}

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowParser[E <: Expr, Alg <: ControlFlowAlg[E]] extends LangParser[E, Alg] {
  def ControlFlow: Rule1[Alg => E] = rule {
    If
  }

  def If = rule {
    "if" ~ Expr ~ TempBlock ~ Else ~> {
      (p: Alg => E, b: Alg => E, o: Alg => E) => (alg: Alg) => alg.elsex(alg.ifx(p(alg), b(alg)), o(alg))} |
    "if" ~ (Expr ~ TempBlock) ~> { (p: Alg => E, b: Alg => E) => (alg: Alg) => alg.ifx(p(alg), b(alg)) }

  }

  def Else = rule {
     "else" ~ TempBlock
  }

  def TempBlock = rule {
    "{" ~ Expr  ~ "}"
  }
}
