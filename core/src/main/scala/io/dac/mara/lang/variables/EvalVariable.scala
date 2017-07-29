package io.dac.mara.lang.variables

import io.dac.mara.core.{MaraValue, Namespace}
import io.dac.mara.phases.{Eval, EvalOp}


/**
  * Created by dcollins on 8/12/16.
  */
trait EvalVariable extends EvalOp with VariableAlg[Eval] with Namespace {
  import MaraValue._

  override def valdeclare(name: String, typex: Option[String]): Eval =
    op { declareValue(name); UnitValueNamed(name) }

  override def valassign(name: String, typex: Option[String], value: Eval) =
    op {
      bindValue(name, value.eval)
    }

  override def valsubstitution(name: String) =
    op { lookupValue(name) }

}
