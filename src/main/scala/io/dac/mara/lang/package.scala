package io.dac.mara

import io.dac.mara.lang.compound.{CompoundAlg, EvalCompound, ShowCompound, TypedCompound}
import io.dac.mara.lang.controlflow.{ControlFlowAlg, EvalControlFlow, ShowControlFlow, TypedControlFlow}
import io.dac.mara.lang.functions.{EvalFunction, FunctionAlg, ShowFunction, TypedFunction}
import io.dac.mara.lang.literals.{EvalLiteral, LiteralAlg, ShowLiteral, TypedLiteral}
import io.dac.mara.lang.operators.{EvalOperator, OperatorAlg, ShowOperator, TypedOperator}
import io.dac.mara.lang.variables.{EvalVariable, ShowVariable, TypedVariable, VariableAlg}

/**
  * Created by dcollins on 3/18/17.
  */
package object lang {
  type CombinedAlg[E] = LiteralAlg[E]
    with OperatorAlg[E]
    with ControlFlowAlg[E]
    with FunctionAlg[E]
    with VariableAlg[E]
    with CompoundAlg[E]

  type CombinedShow =  ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound
  type CombinedEval =  EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound
  type CombinedTyped = TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound

  object alg {
    lazy val show = new Object with ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound
    lazy val eval = new Object with EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound
    lazy val typed = new Object with TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound
  }
}
