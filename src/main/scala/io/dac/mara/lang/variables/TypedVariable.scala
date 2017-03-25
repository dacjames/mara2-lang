package io.dac.mara.lang.variables

import io.dac.mara.core.{MaraType, Namespace}
import io.dac.mara.exprops.{Typed, TypedOp}

import scala.collection.mutable

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedVariable extends TypedOp with VariableAlg[Typed] with Namespace {
  import MaraType._

  override def valdeclare(name: String, typex: Option[String]): Typed = op {
    val typeresult = typex match {
      case None => InferrableType()
      case Some(typename) => lookupType(typename)
    }
    bindType(name, typeresult)
    typeresult
  }

  override def valassign(name: String, typex: Option[String], value: Typed): Typed = op {
    val typeresult = typex match {
      case None => {
        value.typex
      }
      case Some(typename) => {
        val valuetype = value.typex
        if (valuetype.name.contains(typename)) {
          valuetype
        } else {
          ErrorType(s"${valuetype} does not match ${typename}")
        }
      }
    }
    bindType(name, typeresult)

    typeresult
  }

  override def valsubstitution(name: String): Typed = op {
    lookupType(name)
  }

}

