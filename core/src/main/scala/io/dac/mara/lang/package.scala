package io.dac.mara

import io.dac.mara.core.{Namespace, TreeContext}
import io.dac.mara.phases.{Compiled, Factory}
import io.dac.mara.lang.compound.{CompoundAlg, _}
import io.dac.mara.lang.controlflow._
import io.dac.mara.lang.functions._
import io.dac.mara.lang.literals._
import io.dac.mara.lang.operators._
import io.dac.mara.lang.variables._

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
  type CombinedCompiled =  CompiledLiteral with CompiledOperator with CompiledVariable with CompiledFunction with MissingImpl[Compiled]
  type CombinedFactory = FactoryLiteral with FactoryOperator with FactoryControlFlow with FactoryFunction with FactoryVariable with FactoryCompound with MissingImpl[Factory]

  private[this] val langNamespace: Namespace = new Namespace

  object alg {
    def show(implicit context: TreeContext) = new ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound
    def eval(implicit context: TreeContext) = new EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound {
      override def namespace: Namespace = langNamespace
    }
    def typed(implicit context: TreeContext) = new TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound {
      override def namespace: Namespace = langNamespace
    }
    def compiled(implicit context: TreeContext) = new CompiledLiteral with CompiledOperator with CompiledVariable with CompiledFunction with MissingImpl[Compiled]
    def factory(implicit context: TreeContext) = new FactoryLiteral with FactoryOperator with FactoryControlFlow with FactoryFunction with FactoryVariable with FactoryCompound with MissingImpl[Factory]
  }
}
