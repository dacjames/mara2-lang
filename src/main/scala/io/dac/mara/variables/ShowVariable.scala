package io.dac.mara.variables

import io.dac.mara.exprops.{Show, ShowOp}

/**
  * Created by dcollins on 8/12/16.
  */
trait ShowVariable extends ShowOp with VariableAlg[Show] {
  override def valdeclare(name: String, typex: Option[String]): Show = typex match {
    case Some(typex) => op { s"val ${name}: ${typex}" }
    case None => op { s"val ${name}" }
  }

  override def valassign(name: String, typex: Option[String], value: Show) =typex match {
    case Some(typex) => op { s"val ${name}: ${typex} = ${value.show}" }
    case None => op { s"val ${name} = ${value.show}" }
  }

  override def substitution(name: String) =
    op { name }
}
