package io.dac.mara.lang.functions

import io.dac.mara.core.MaraAttr.CodeAttr
import io.dac.mara.core._
import io.dac.mara.ir.IrFragment
import io.dac.mara.phases.{Compiled, CompiledOp}

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
                           body: Seq[Compiled]): Compiled = op {

    val paramlist = valparams.map {
      case Pair(name, typeOpt) => s"i32 %$name"
    }.mkString(",")

    val bodyFragment = Compiled.recurse(body)

    val bytecode =
      (define(name, s"define i32 @$name($paramlist) {") :+
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
