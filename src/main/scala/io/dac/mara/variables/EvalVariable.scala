package io.dac.mara.variables

import scala.collection.mutable

import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/12/16.
  */
trait EvalVariable extends EvalOp with VariableAlg[Eval] {
  private val namespace: mutable.Map[String, Option[Int]] = mutable.Map.empty

  override def valdeclare(name: String, typex: Option[String]): Eval =
    op { namespace += (name -> None); 0 }

  override def valassign(name: String, typex: Option[String], value: Eval) =
    op {
      val result = value.eval
      namespace += (name -> Some(result))
      result
    }

  override def substitution(name: String) =
    op {
      val whenUnassigned = 0
      val whenUndeclared = 0
      namespace.get(name).map(_.getOrElse(whenUndeclared)) getOrElse whenUnassigned
    }

  override def block(e1: Eval, e2: Eval) =
    op {
      e1.eval; e2.eval
    }
}
