package io.dac.mara.lang.variables

import scala.collection.mutable
import io.dac.mara.core.{MaraValue, Namespace}
import io.dac.mara.core.MaraValue._
import io.dac.mara.exprops.{Eval, EvalOp, Show}

/**
  * Created by dcollins on 8/12/16.
  */
trait EvalVariable extends EvalOp with VariableAlg[Eval] with Namespace {
  import MaraValue.implicits._

  override def valdeclare(name: String, typex: Option[String]): Eval =
    op { declareValue(name); UnitValueNamed(name) }

  override def valassign(name: String, typex: Option[String], value: Eval) =
    op {
      bindValue(name, value.eval)
    }

  override def valsubstitution(name: String) =
    op { lookupValue(name) }

  override def empty = op { EmptyValue() }

  private[this] val blockPartial =
    new PartialFunction[Eval, MaraValue] {
      private[this] var it: MaraValue = _
      override def isDefinedAt(x: Eval) = { it = x.eval; ! it.isInstanceOf[EmptyValue]}
      override def apply(v1: Eval) = it
    }

  override def block(exprs: Seq[Eval]) =
    op {
      exprs.collect(blockPartial).last
    }
}
