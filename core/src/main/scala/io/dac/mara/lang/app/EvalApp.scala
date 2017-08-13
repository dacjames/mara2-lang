package io.dac.mara.lang.app

import io.dac.mara.core.Pair
import io.dac.mara.lang.functions.FunctionAlg
import io.dac.mara.phases.{Eval, EvalOp}

trait EvalApp extends EvalOp with AppAlg[Eval] with FunctionAlg[Eval] {
  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Eval]): Eval =
    defconcrete(name, Seq.empty, args, None, body)
}
