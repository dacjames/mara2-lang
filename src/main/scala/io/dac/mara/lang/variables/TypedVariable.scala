package io.dac.mara.lang.variables

import io.dac.mara.core.MaraType
import io.dac.mara.exprops.{Typed, TypedOp}
import scala.collection.mutable

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedVariable extends TypedOp with VariableAlg[Typed] {
  import MaraType._

  private[this] val namespace: mutable.Map[String, MaraType] = mutable.Map.empty[String, MaraType]
  private[this] val builtins: Map[String, MaraType] = Map(
    "String" -> StringType(),
    "Int" -> IntType(),
    "Bool" -> BoolType()
  )

  require(builtins.forall {
    case (name, typex) => typex.name.contains(name)
  })

  override def valdeclare(name: String, typex: Option[String]): Typed = op {
    typex match {
      case None => InferableType()
      case Some(typename) => {
        val typeresult = builtins.get(typename) match {
          case Some(t) => t
          case None => ErrorType(typename)
        }

        namespace += (name -> typeresult)

        typeresult
      }
    }
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
    namespace += (name -> typeresult)

    typeresult
  }

  override def valsubstitution(name: String): Typed = op {
    namespace(name)
  }

  override def block(exprs: Seq[Typed]): Typed = op {
    exprs.reduce{ (a, b) => { a.typex; b} }.typex
  }

}

