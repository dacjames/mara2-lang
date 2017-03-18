package io.dac.mara

import io.dac.mara.core.Expr
import io.dac.mara.lang.controlflow.{ControlFlowAlg, EvalControlFlow, ShowControlFlow, TypedControlFlow}
import io.dac.mara.lang.functions.{EvalFunction, FunctionAlg, ShowFunction, TypedFunction}
import io.dac.mara.lang.literals.{EvalLiteral, LiteralAlg, ShowLiteral, TypedLiteral}
import io.dac.mara.lang.operators.{EvalOperator, OperatorAlg, ShowOperator, TypedOperator}
import io.dac.mara.lang.variables.{EvalVariable, ShowVariable, TypedVariable, VariableAlg}

/**
  * Created by dcollins on 3/18/17.
  */
package object lang {
  type CombinedAlg[E <: Expr] = LiteralAlg[E]
    with OperatorAlg[E]
    with ControlFlowAlg[E]
    with FunctionAlg[E]
    with VariableAlg[E]

  type CombinedShow =  ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable
  type CombinedEval =  EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable
  type CombinedTyped = TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable

  object alg {
    lazy val show = new Object with ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable
    lazy val eval = new Object with EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable
    lazy val typed = new Object with TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable
  }
}
