package io.dac.mara.lang.functions

import io.dac.mara.exprops.{Show, ShowOp}

/**
  * Created by dcollins on 8/28/16.
  */
trait ShowFunction extends ShowOp with FunctionAlg[Show] {

  private[this] def showFunction(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String], body: Seq[Show]) = {
    val typepart = typex.map(t => s" -> $t").getOrElse("")
    def parampart(them: Seq[Param]) =
      them.map {
        case (n, Some(t)) => s"${n}: ${t}"
        case (n, None) => s"$n"
      }.mkString(", ")

    s"def ${name}(${parampart(typeparams)})(${parampart(valparams)})$typepart { ${body.map(_.show).mkString("; ")} }"
  }


  override def defconcrete(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String], body: Seq[Show]): Show = op {
    showFunction(name, typeparams, valparams, typex, body)
  }


  override def call(name: String, args: Seq[Show]): Show = op {
    s".${name}(${args.map(_.show).mkString(", ")})"
  }

}
