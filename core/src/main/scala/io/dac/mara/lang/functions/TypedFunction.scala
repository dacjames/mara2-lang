package io.dac.mara.lang.functions
import io.dac.mara.core.{MaraType, Namespace, Record}
import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.lang.variables.VariableAlg

/**
  * Created by dcollins on 8/28/16.
  */
trait TypedFunction extends TypedOp with FunctionAlg[Typed] with VariableAlg[Typed] with Namespace {
  import MaraType._

  override def defconcrete(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String], body: Seq[Typed]) = op {

    // TODO: Function Type Parameters are ignored

    val tags = valparams.map {
      case (name, typex) =>
        typex match {
          case Some(t) => (name, lookupType(t))
          case None => (name, InferrableType())
        }
    }

    val input = RecordType(Record(tags: _*))

    val withPreample = valparams.map{
      case (name, typeOpt) => valdeclare(name, typeOpt)
    } ++ body

    val output =
      if (withPreample.isEmpty) UnitType()
      else MaraType.promote(withPreample.map(_.typex).last)

    def newFuncType =
      bindType(name, FunctionType(input, output))

    val funcType = typex match {
      case Some(t) =>
        if (isSubtype(output, lookupType(t))) newFuncType
        else ErrorType(s"Defined type ${t} does not inferred type ${output}")
      case None =>
        newFuncType
    }
    bindType(name, funcType)
  }

  override def call(name: String, args: Seq[Typed]) = op {
    val funcType = lookupType(name)
    val argsTags = args.zipWithIndex.map{
      case (arg, index) => (index, arg.typex)
    }

    val argsType = RecordType(Record(argsTags: _*))

    funcType match {
      case FunctionType(input, output) =>
        if (isSubtype(argsType, input)) output
        else ErrorType(s"Args ${argsType} does not match parameters ${input} calling function ${name}")
      case _ => ErrorType(s"${name} is not callable")
    }
  }

}
