package io.dac.mara

import io.dac.mara.lang.controlflow.{ControlFlowAlg, ControlFlowParser}
import io.dac.mara.core.Expr
import io.dac.mara.lang.functions.{FunctionAlg, FunctionParser}
import io.dac.mara.lang.literals.{LiteralAlg, LiteralParser}
import io.dac.mara.lang.operators.{OperatorAlg, OperatorParser}
import io.dac.mara.lang.parsers.ParensParser
import io.dac.mara.lang.variables.{VariableAlg, VariableParser}
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E <: Expr, Alg <: LiteralAlg[E]
                            with OperatorAlg[E]
                            with ControlFlowAlg[E]
                            with FunctionAlg[E]
                            with VariableAlg[E]]
  extends Parser with LiteralParser[E, Alg]
    with OperatorParser[E, Alg]
    with ControlFlowParser[E, Alg]
    with VariableParser[E, Alg]
    with ParensParser[E, Alg]
    with FunctionParser[E, Alg] {

  def Root: Rule1[Alg => E] = InputLine

  def InputLine: Rule1[Alg => E] = rule { (Expr | Terminal) ~ EOI }

  def Expr: Rule1[Alg => E] = rule {
    Operator |
    ControlFlow |
    Function |
    Variable |
    Substitution |
    Call
  }


  def Terminal: Rule1[Alg => E] = rule { Literal | Parens | Do }

}

