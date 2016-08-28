package io.dac.mara.lang.functions


import io.dac.mara.core.{MaraType, MaraValue}
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/28/16.
  */
trait EvalFunction extends EvalOp with FunctionAlg[Eval] {
  import MaraValue._
  import MaraType._

  private[this] val builtins: Map[String, MaraType] = Map(
    "String" -> StringType(),
    "Int" -> IntType(),
    "Bool" -> BoolType()
  )

  override def valparam(name: String, typex: Option[String]): Eval = op {
    StringValue(name)
  }

  override def typeparam(name: String, typex: Option[String]): Eval = op {
    StringValue(name)
  }

  override def defconcrete(name: String, typeparams: Seq[Eval], valparams: Seq[Eval], typex: Option[String], body: Seq[Eval]): Eval = op {
    FunctionValue(name=name)
  }

  override def defabstract(name: String, typeparams: Seq[Eval], valparams: Seq[Eval], typex: Option[String]): Eval = op {
    FunctionValue(name=name)
  }

}
