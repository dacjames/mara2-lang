package io.dac.mara

import io.dac.mara.core.PhaseContext
import io.dac.mara.exprops.{Compiled, Typed}
import io.dac.mara.lang.compound.CompoundAlg
import io.dac.mara.lang.controlflow._
import io.dac.mara.lang.functions._
import io.dac.mara.lang.literals._
import io.dac.mara.lang.operators._
import io.dac.mara.lang.variables._
import io.dac.mara.lang.compound._

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

  trait MissingImpl[E]
    extends LiteralAlg[E]
      with OperatorAlg[E]
      with ControlFlowAlg[E]
      with FunctionAlg[E]
      with VariableAlg[E]
      with CompoundAlg[E]

  type CombinedShow =  ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound
  type CombinedEval =  EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound
  type CombinedTyped = TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound
  type CombinedCompiled = MissingImpl[Compiled] with CompiledLiteral with CompiledOperator with CompiledVariable with CompiledFunction


  object alg {
    def show(implicit context: PhaseContext) = new ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound
    def eval(implicit context: PhaseContext) = new EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound
    def typed(implicit context: PhaseContext) = new TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound

    def compiled(implicit context: PhaseContext) = new CompiledLiteral with CompiledOperator with CompiledVariable with CompiledFunction with MissingImpl[Compiled]
  }
}
