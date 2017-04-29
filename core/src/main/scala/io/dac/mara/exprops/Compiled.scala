package io.dac.mara.exprops

import io.dac.mara.core.Expr.Replable
import io.dac.mara.core.Expr

/**
  * Created by dcollins on 4/28/17.
  */
trait Compiled extends Expr {
  def bytecode: Seq[String]
  def result: String
}

object Compiled {
  implicit object CompileRepl extends Replable[Compiled, String] {
    override def value(e: Compiled) = e.bytecode.mkString("\n")
  }
}

trait CompiledOp {
  def op(f: => (Seq[String], String)): Compiled =
    new Compiled {
      override def bytecode: Seq[String] = f._1
      override def result = f._2
    }
}
