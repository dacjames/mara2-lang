package io.dac.mara.lang.functions

import io.dac.mara.exprops.{Compiled, CompiledOp, Typed}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledFunction extends CompiledOp with FunctionAlg[Compiled] {
  def typedAlg: FunctionAlg[Typed]

  override def defconcrete(name: String,
                           typeparams: Seq[(String, Option[String])],
                           valparams: Seq[(String, Option[String])],
                           typex: Option[String],
                           body: Seq[Compiled]): Compiled = op {

    val paramlist = valparams.map {
      case (name, typeOpt) => s"i32 %$name"
    }.mkString(",")

    val bytecode = Seq(
      s"define i32 @$name($paramlist) {",
      "entry:"
    ) ++ body.flatMap(_.bytecode) ++
    Seq(
      s"ret i32 ${body.last.result};",
      "}"
    )

    val result = s"@$name"

    (bytecode, result)
  }

}
