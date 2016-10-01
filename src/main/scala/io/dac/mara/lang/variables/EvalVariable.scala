package io.dac.mara.lang.variables

import scala.collection.mutable
import io.dac.mara.core.{MaraValue, Namespace}
import io.dac.mara.core.MaraValue._
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/12/16.
  */
trait EvalVariable extends EvalOp with VariableAlg[Eval] with Namespace {
  import MaraValue.implicits._

  override def valdeclare(name: String, typex: Option[String]): Eval =
    op { declareValue(name); 0 }

  override def valassign(name: String, typex: Option[String], value: Eval) =
    op {
      bindValue(name, value.eval)
    }

  override def valsubstitution(name: String) =
    op { lookupValue(name) }

  override def block(exprs: Seq[Eval]) =
    op {
      exprs.reduce{ (a, b) => {a.eval; b} }.eval
    }
}
