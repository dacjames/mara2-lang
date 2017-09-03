package io.dac.mara.lang.controlflow

import io.dac.mara.lang.parsers.{BlockParser, LangParser}
import org.parboiled2.Rule1

/**
  * Created by dcollins on 8/6/16.
  */
trait ControlFlowParser[E, Alg <: ControlFlowAlg[E]] extends LangParser[E, Alg] with BlockParser[E, Alg] {
  def ControlFlow: Rule1[E] = rule {
    If
  }

  def If = rule {
    "if" ~ Expr ~ Block ~ Else ~> { (p: E, b: Seq[E], o: Seq[E]) =>
      alg.ifelse(p, b, o)
    } |
    "if" ~ (Expr ~ Block) ~> { (p: E, b: Seq[E]) =>
      alg.ifelse(p, b, Seq(alg.empty))
    }
  }

  def Else = rule {
     "else" ~ Block
  }
//
//  def TempBlock = rule {
//    "{" ~ Expr  ~ "}"
//  }
}
