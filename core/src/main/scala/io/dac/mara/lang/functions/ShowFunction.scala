package io.dac.mara.lang.functions

import io.dac.mara.core._
import io.dac.mara.phases.{Show, ShowOp}

/**
  * Created by dcollins on 8/28/16.
  */
trait ShowFunction extends ShowOp with FunctionAlg[Show] {

  private[this] def showFunction(name: String, typeparams: Seq[Pair], valparams: Seq[Pair], typex: Option[String], body: Seq[Show]) = {
    val typepart = typex.map(t => s" -> $t").getOrElse("")
    def parampart(them: Seq[Pair]) =
      them.map {
        case Pair(n, Some(t)) => s"${n}: ${t}"
        case Pair(n, None) => s"$n"
      }.mkString(", ")

    s"def ${name}(${parampart(typeparams)})(${parampart(valparams)})$typepart { ${body.map(_.show).mkString("; ")} }"
  }


  override def defconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[Show]): Show = op {
    showFunction(name, typeparams, valparams, typex, body)
  }


  override def call(name: String, args: Seq[Show]): Show = op {
    s".${name}(${args.map(_.show).mkString(", ")})"
  }

}
