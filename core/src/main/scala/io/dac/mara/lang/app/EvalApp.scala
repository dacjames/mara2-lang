package io.dac.mara.lang.app

import io.dac.mara.core.MaraAttr.{ErrorAttr, ExecutableAttr}
import io.dac.mara.core.MaraValue.{ErrorValue, ExecutableValue}
import io.dac.mara.core.{MaraValue, NamespaceLookup, Pair}
import io.dac.mara.lang.functions.FunctionAlg
import io.dac.mara.phases.{Eval, EvalOp}

trait EvalApp extends EvalOp with AppAlg[Eval] with FunctionAlg[Eval] with NamespaceLookup {
  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Eval]): Eval = op {
    lookupAttr[ExecutableAttr](name) match {
      case ExecutableAttr(executable: MaraValue) =>
        bindValue(name, executable)
      case ErrorAttr(msg) =>
        ErrorValue(msg)
    }
  }
}
