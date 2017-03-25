package io.dac.mara

import io.dac.mara.lang.compound.CompoundParser
import io.dac.mara.lang.controlflow.ControlFlowParser
import io.dac.mara.lang.functions.FunctionParser
import io.dac.mara.lang.literals.LiteralParser
import io.dac.mara.lang.operators.OperatorParser
import io.dac.mara.lang.parsers.ParensParser
import io.dac.mara.lang.variables.VariableParser
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E, Alg <: lang.CombinedAlg[E]  ]
  extends Parser with LiteralParser[E, Alg]
    with OperatorParser[E, Alg]
    with ControlFlowParser[E, Alg]
    with VariableParser[E, Alg]
    with ParensParser[E, Alg]
    with FunctionParser[E, Alg]
    with CompoundParser[E, Alg] {

  def Root: Rule1[E] = InputLine

  def InputLine: Rule1[E] = rule { (Expr | Terminal) ~ EOI }

  def Expr: Rule1[E] = rule {
    Operator | Terminal | Empty
  }

  def Terminal: Rule1[E] = rule {
    Parens | Literal | Do | ControlFlow |  Function | Variable | Substitution | Call | List | Record
  }

}

