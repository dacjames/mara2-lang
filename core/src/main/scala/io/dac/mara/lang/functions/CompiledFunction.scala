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
  import io.dac.mara.ir.implicits._
  import io.dac.mara.core.MaraType._


  override def funconcrete(name: String,
                           typeparams: Seq[Pair.Type],
                           valparams: Seq[Pair.Value],
                           typex: Option[String],
                           body: Seq[Compiled]): Compiled = op {

    val paramlist = valparams.map {
      case Pair(name, typeOpt) => s"i32 %$name"
    }.mkString(",")

    val (bodyCode, bodyResult) = Compiled.recurse(body)

    val bytecode =
      s"define i32 @$name($paramlist) {" /|
      "entry:" ++ bodyCode ++
      s"ret i32 $bodyResult" /|
      "}"

    val result = s"@$name"

    bindAttr(
      name,
      CodeAttr(bytecode.mkString("\n"))
    )

    addSymbol(name, bytecode)

    (bytecode, result)
  }

  override def call(name: String, args: Seq[Compiled]): Compiled = op {
    lookupType(name) match {
      case FunctionType(RecordType(input), output) =>
        val (prefixCode, argFragment) =
          if (input.keys.isEmpty) (Vector.empty[IrFragment], "")
          else {
            val prefix = mutable.ArrayBuffer.empty[IrFragment]
            var results = mutable.ArrayBuffer.empty[IrFragment]

            args.foreach { compiled =>
              prefix ++= compiled.bytecode
              results += compiled.result
            }

            val loweredInputTypes = input.values.flatMap(MaraType.lower)
            val fragment = ((loweredInputTypes zip results) map {
              case (llvmType, result) => s"$llvmType $result"
            }).mkString(",")

            (prefix.toVector, fragment)
          }
        val result = nextTemp()
        val bytecode =
          prefixCode /|
          s"$result = call ${MaraType.lower(output).get} @$name($argFragment)"

        (bytecode, result)
    }
  }
}
