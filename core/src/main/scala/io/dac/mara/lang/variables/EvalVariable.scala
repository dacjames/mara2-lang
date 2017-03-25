package io.dac.mara.lang.variables

import io.dac.mara.core.Namespace
import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.core.MaraValue


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
