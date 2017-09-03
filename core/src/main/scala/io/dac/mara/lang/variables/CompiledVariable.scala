package io.dac.mara.lang.variables

import io.dac.mara.core.MaraAttr.{ErrorAttr, RefAttr}
import io.dac.mara.core._
import io.dac.mara.phases.{Compiled, CompiledOp, Typed}

/**
  * Created by dcollins on 4/28/17.
  */
trait CompiledVariable extends CompiledOp with VariableAlg[Compiled] with ModuleLookup with NamespaceLookup {
  import io.dac.mara.ir.IrModel._

  override def valdeclare(name: String, typex: Option[String]): Compiled = op {
    Fragment.empty
  }

  override def valassign(name: String, typex: Option[String], value: Compiled): Compiled = opWith[Typed] { typex =>
    val variableType = MaraType.lower(typex).get
    val pointerType = s"$variableType*"

    val initCode = value.fragment

    val allocResult = l(nextTemp())

    bindAttr[RefAttr](name, RefAttr(allocResult))

    val bytcode = initCode :+
      stmt(allocResult, r(s"alloca ${variableType}")) :+
      stmt(s"store ${variableType} ${initCode.result}, ${pointerType} ${allocResult}")

    bytcode
  }

  override def valsubstitution(name: String): Compiled = op {
    lookupAttr[RefAttr](name) match {
      case ErrorAttr(_) =>
        // Function arguments don't have references
        // So access them by name
        Fragment(l(s"%$name"))

      case RefAttr(lval) =>
        val typex = lookupType(name)

        val variableType = MaraType.lower(typex).get
        val pointerType = s"$variableType*"

        lval
        stmt(l(nextTemp()), r(s"load ${variableType}, ${pointerType} ${lval}"))
    }
  }


}
