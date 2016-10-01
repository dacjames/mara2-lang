package io.dac.mara.lang.functions

import io.dac.mara.exprops.{Show, ShowOp}

/**
  * Created by dcollins on 8/28/16.
  */
trait ShowFunction extends ShowOp with FunctionAlg[Show] {

  private[this] def showFunction(name: String, typeparams: Seq[Show], valparams: Seq[Show], typex: Option[String], body: Seq[Show]) = {
    val typepart = typex.map(t => s" -> $t").getOrElse("")
    s"def ${name}(${typeparams.map(_.show).mkString(", ")})(${valparams.map(_.show).mkString(", ")})$typepart { ${body.map(_.show).mkString("; ")} }"
  }


  override def defabstract(name: String, typeparams: Seq[Show], valparams: Seq[Show], typex: Option[String]): Show = op {
    showFunction(name, typeparams, valparams, typex, Seq.empty)
  }

  override def defconcrete(name: String, typeparams: Seq[Show], valparams: Seq[Show], typex: Option[String], body: Seq[Show]): Show = op {
    showFunction(name, typeparams, valparams, typex, body)
  }

  override def typeparam(name: String, bounds: Option[String]): Show = op {
    bounds match {
      case Some(bounds) => s"${name}: ${bounds}"
      case None => name
    }
  }

  override def valparam(name: String, typex: Option[String]): Show = op {
    typex match {
      case Some(typex) => s"${name}: ${typex}"
      case None => name
    }
  }

  override def call(name: String, args: Seq[Show]): Show = op {
    s".${name}(${args.map(_.show).mkString(", ")})"
  }

}
