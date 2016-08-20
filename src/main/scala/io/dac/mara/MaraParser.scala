package io.dac.mara

import io.dac.mara.controlflow.{ControlFlowAlg, ControlFlowParser}
import io.dac.mara.core.Expr
import io.dac.mara.literals.{LiteralAlg, LiteralParser}
import io.dac.mara.operators.{OperatorAlg, OperatorParser}
import io.dac.mara.variables.{VariableAlg, VariableParser}
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E <: Expr, T <: LiteralAlg[E]
                            with OperatorAlg[E]
                            with ControlFlowAlg[E]
                            with VariableAlg[E]]
  extends Parser with LiteralParser[E, T]
                 with OperatorParser[E, T]
                 with ControlFlowParser[E, T]
                 with VariableParser[E, T] {

  def Root = InputLine

  def InputLine = rule { (Expr | Terminal) ~ EOI }

  def Expr: Rule1[E] = rule { Operator | ControlFlow | Variable | Substitution }

  def Terminal: Rule1[E] = rule { Literal | Parens | Block }

  def Parens = rule { '(' ~ Expr ~ ')' }

}
