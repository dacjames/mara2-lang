package io.dac.mara

import io.dac.mara.core.{Empty, Module, Namespace, TreeContext}
import io.dac.mara.lang.app._
import io.dac.mara.phases._
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
    with AppAlg[E]

  trait MissingImpl[E]
    extends LiteralAlg[E]
      with OperatorAlg[E]
      with ControlFlowAlg[E]
      with FunctionAlg[E]
      with VariableAlg[E]
      with CompoundAlg[E]
      with AppAlg[E]

  type CombinedShow =  ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound with ShowApp with MissingImpl[Show]
  type CombinedEval =  EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound with EvalApp with MissingImpl[Eval]
  type CombinedTyped = TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound with TypedApp with MissingImpl[Typed]
  type CombinedCompiled =  CompiledLiteral with CompiledOperator with CompiledVariable with CompiledCompound with CompiledFunction with CompiledApp with MissingImpl[Compiled]
  type CombinedFactory = FactoryLiteral with FactoryOperator with FactoryControlFlow with FactoryFunction with FactoryVariable with FactoryCompound with FactoryApp
  type CombinedStaged = StagedCompound with StagedApp with StagedFunction with MissingImpl[Staged]

  private[this] val langNamespace: Namespace = new Namespace
  private[this] val langModule: Module = new Module

  def clear(): Unit = {
    langNamespace.clear()
    langModule.clear()
  }

  object alg {
    def show(implicit context: TreeContext) = new ShowLiteral with ShowOperator with ShowControlFlow with ShowFunction with ShowVariable with ShowCompound with ShowApp with MissingImpl[Show] {
      override val emptyEvidence: Empty[Show] = Show.ShowEmpty
    }
    def eval(implicit context: TreeContext) = new EvalLiteral with EvalOperator with EvalControlFlow with EvalFunction with EvalVariable with EvalCompound with EvalApp with MissingImpl[Eval] {
      override val emptyEvidence: Empty[Eval] = Eval.EvalEmpty
      override val namespace: Namespace = langNamespace
    }
    def typed(implicit context: TreeContext) = new TypedLiteral with TypedOperator with TypedControlFlow with TypedFunction with TypedVariable with TypedCompound with TypedApp with MissingImpl[Typed] {
      override val emptyEvidence: Empty[Typed] = Typed.TypedEmpty
      override val namespace: Namespace = langNamespace
    }
    def compiled(implicit context: TreeContext) = new CompiledLiteral with CompiledOperator with CompiledVariable with CompiledCompound with CompiledFunction with CompiledApp with MissingImpl[Compiled] {
      override val emptyEvidence: Empty[Compiled] = Compiled.CompiledEmpty
      override val namespace: Namespace = langNamespace
      override val module: Module = langModule
    }
    def factory(implicit context: TreeContext) = new FactoryLiteral with FactoryOperator with FactoryControlFlow with FactoryFunction with FactoryVariable with FactoryCompound with FactoryApp {
      override val emptyEvidence: Empty[Factory] = Factory.FactoryEmpty
    }

    def staged(implicit context: TreeContext) = new StagedCompound with StagedApp with StagedFunction with MissingImpl[Staged] {
      override val emptyEvidence: Empty[Staged] = Staged.StagedEmpty
      override val namespace: Namespace = langNamespace
      override val module: Module = langModule
    }


  }
}
