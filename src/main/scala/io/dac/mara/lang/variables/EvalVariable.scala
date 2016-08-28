package io.dac.mara.lang.variables

import scala.collection.mutable
import io.dac.mara.core.MaraValue
import io.dac.mara.core.MaraValue._
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/12/16.
  */
trait EvalVariable extends EvalOp with VariableAlg[Eval] {
  import MaraValue.implicits._

  private val namespace: mutable.Map[String, Option[MaraValue]] = mutable.Map.empty

  override def valdeclare(name: String, typex: Option[String]): Eval =
    op { namespace += (name -> None); 0 }

  override def valassign(name: String, typex: Option[String], value: Eval) =
    op {
      val result = value.eval
      namespace += (name -> Some(result))
      result
    }

  override def valsubstitution(name: String) =
    op {
      val whenUnassigned = ErrorValue(s"Unassigned variable ${name}")
      val whenUndeclared = ErrorValue(s"Undeclared variable ${name}")
      namespace.get(name).map(_.getOrElse(whenUndeclared)) getOrElse whenUnassigned
    }

  override def block(exprs: Seq[Eval]) =
    op {
      exprs.reduce{ (a, b) => {a.eval; b} }.eval
    }
}
