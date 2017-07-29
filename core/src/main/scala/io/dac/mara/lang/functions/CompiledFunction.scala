package io.dac.mara.lang.functions

import io.dac.mara.exprops.{Compiled, CompiledOp, Typed}
import io.dac.mara.ir.IrFragment

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledFunction extends CompiledOp with FunctionAlg[Compiled] {
  import io.dac.mara.ir.implicits._

  override def defconcrete(name: String,
                           typeparams: Seq[(String, Option[String])],
                           valparams: Seq[(String, Option[String])],
                           typex: Option[String],
                           body: Seq[Compiled]): Compiled = op {

    val paramlist = valparams.map {
      case (name, typeOpt) => s"i32 %$name"
    }.mkString(",")

    val bytecode =
      s"define i32 @$name($paramlist) {" /|
      "entry:" ++
      body.flatMap(_.bytecode).toVector ++
      s"ret i32 ${body.last.result};" /|
      "}"

    val result = s"@$name"

    (bytecode, result)
  }

}
