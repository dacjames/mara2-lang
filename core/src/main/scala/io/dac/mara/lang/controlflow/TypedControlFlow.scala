package io.dac.mara.lang.controlflow

import io.dac.mara.core.MaraType
import io.dac.mara.core.MaraType.AnyType
import io.dac.mara.phases.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedControlFlow extends TypedOp with ControlFlowAlg[Typed] {
  override def ifx(pred: Typed, body: Typed): Typed = op {
    AnyType()
  }

  override def elsex(expr: Typed, otherwise: Typed): Typed = op {
    AnyType()
  }

  override def ifelse(pred: Typed, ifbody: Seq[Typed], elsebody: Seq[Typed]) = op {
    val ifType = ifbody.last.typex
    val ifPromoted = MaraType.promote(ifType)
    val elseType = elsebody.last.typex
    val elsePromoted = MaraType.promote(elseType)

    if (ifType == elseType) ifType
    else if (MaraType.isSubtype(ifType, elseType)) elseType
    else if (MaraType.isSubtype(elseType, ifType)) ifType
    else if (MaraType.isSubtype(ifType, elsePromoted)) elsePromoted
    else if (MaraType.isSubtype(elseType, ifPromoted)) ifPromoted
    else MaraType.ErrorType(s"Incompatible types in branches of ifelse: ${ifType}, ${elseType}")
  }
}
