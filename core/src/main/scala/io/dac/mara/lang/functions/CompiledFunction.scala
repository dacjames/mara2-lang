package io.dac.mara.lang.functions

import io.dac.mara.core.MaraAttr.CodeAttr
import io.dac.mara.core._
import io.dac.mara.phases.{Compiled, CompiledOp}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledFunction extends CompiledOp with FunctionAlg[Compiled] with NamespaceLookup {
  import io.dac.mara.ir.implicits._

  override def defconcrete(name: String,
                           typeparams: Seq[Pair.Type],
                           valparams: Seq[Pair.Value],
                           typex: Option[String],
                           body: Seq[Compiled]): Compiled = op {

    val paramlist = valparams.map {
      case Pair(name, typeOpt) => s"i32 %$name"
    }.mkString(",")

    val bytecode =
      s"define i32 @$name($paramlist) {" /|
      "entry:" ++
      body.flatMap(_.bytecode).toVector ++
      s"ret i32 ${body.last.result};" /|
      "}"

    val result = s"@$name"

    bindAttr(
      name,
      "code",
      CodeAttr(bytecode.mkString("\n"))
    )

    (bytecode, result)
  }

}
