package io.dac.mara.lang.variables

import io.dac.mara.core.MaraType
import io.dac.mara.exprops.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedVariable extends TypedOp with VariableAlg[Typed] {
  import MaraType._

  override def valdeclare(name: String, typex: Option[String]): Typed = op {
    typex match {
      case None => InferableType()
      case Some(typename) => ???
    }
  }

  override def valassign(name: String, typex: Option[String], value: Typed): Typed = op {
    typex match {
      case None => value.typex
      case Some(typename) => {
        val valuetype = value.typex
        if (valuetype.name.contains(typename)) {
          valuetype
        } else {
          TypeError(s"${valuetype} does not match ${typename}")
        }
      }
    }
  }

  override def valsubstitution(name: String): Typed = op { ??? }

}

