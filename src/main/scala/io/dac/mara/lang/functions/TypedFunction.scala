package io.dac.mara.lang.functions

import io.dac.mara.core.{GenSym, Namespace}
import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.lang.variables.VariableAlg

/**
  * Created by dcollins on 8/28/16.
  */
trait TypedFunction extends TypedOp with FunctionAlg[Typed] with VariableAlg[Typed] with Namespace {
  import io.dac.mara.core.MaraType._

  override def defconcrete(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String], body: Seq[Typed]) = op {

    // TODO: Function Type Parameters are ignored

    val input = RecordType(valparams.map {
      case (name, typex) =>
        typex match {
          case Some(t) => lookupType(t)
          case None => InferrableType()
        }
    })

    val withPreample = valparams.map{
      case (name, typeOpt) => valdeclare(name, typeOpt)
    } ++ body

    val output =
      if (withPreample.isEmpty) UnitType()
      else withPreample.map(_.typex).last

    def newFuncType =
      bindType(name, FunctionType(input, output))

    val funcType = typex match {
      case Some(t) =>
        if (output == lookupType(t)) newFuncType
        else ErrorType(s"Defined type ${t} does not inferred type ${output}")
      case None =>
        newFuncType
    }
    bindType(name, funcType)
  }

  override def call(name: String, args: Seq[Typed]) = op {
    val funcType = lookupType(name)
    val argsType = RecordType(args.map(_.typex))

    funcType match {
      case FunctionType(input, output) =>
        if (argsType == input) output
        else ErrorType(s"Args ${argsType} does not match parameters ${input} calling function ${name}")
      case _ => ErrorType(s"${name} is not callable")
    }
  }

}
