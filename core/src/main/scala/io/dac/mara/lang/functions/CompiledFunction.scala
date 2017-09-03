package io.dac.mara.lang.functions

import io.dac.mara.core.MaraAttr.CodeAttr
import io.dac.mara.core._
import io.dac.mara.lang.variables.VariableAlg
import io.dac.mara.phases.{Compiled, CompiledOp, Typed}

import scala.collection.mutable

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledFunction extends CompiledOp with FunctionAlg[Compiled] with NamespaceLookup with ModuleLookup {
  import io.dac.mara.ir.IrModel._
  import io.dac.mara.core.MaraType._


  override def funconcrete(name: String,
                           typeparams: Seq[Pair.Type],
                           valparams: Seq[Pair.Value],
                           typex: Option[String],
                           body: Seq[Compiled]): Compiled = opWith[Typed] { typex =>

    val paramlist = valparams.map {
      case Pair(name, typeOpt) => s"${typeOpt.flatMap(t => MaraType.lower(lookupType(t))).get} %$name"
    }.mkString(",")


    val bodyFragment = Compiled.recurse(body)

    val resultType = MaraType.lower(typex).get

    val bytecode =
      (define(name, s"define ${resultType} @$name($paramlist) {") :+
      stmt("entry:")) ++
      bodyFragment :+
      stmt(s"ret i32 ${bodyFragment.result}") :+
      stmt("}")

    bindAttr(
      name,
      CodeAttr(bytecode)
    )

    addSymbol(name, bytecode)

    bytecode
  }

  override def call(name: String, args: Seq[Compiled]): Compiled = op {
    lookupType(name) match {
      case FunctionType(RecordType(input), output) =>
        val (prefixCode, argFragment) =
          if (input.keys.isEmpty) (Fragment.empty, "")
          else {
            val argCode = args.map(_.fragment).toVector
            val results = argCode.map(_.result)

            val loweredInputTypes = input.values.flatMap(MaraType.lower)
            val fragment = ((loweredInputTypes zip results) map {
              case (llvmType, result) => s"$llvmType $result"
            }).mkString(",")

            (argCode.foldLeft(Fragment.empty)(_ ++ _), fragment)
          }
        val result = nextTemp()
        val bytecode =
          prefixCode :+
          stmt(l(result), r(s"call ${MaraType.lower(output).get} @$name($argFragment)"))

        bytecode
    }
  }
}
