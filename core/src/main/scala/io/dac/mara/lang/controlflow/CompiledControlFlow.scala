package io.dac.mara.lang.controlflow

import io.dac.mara.core.{MaraType, ModuleLookup, NamespaceLookup}
import io.dac.mara.phases.{Compiled, CompiledOp, Typed}

trait CompiledControlFlow extends CompiledOp with ControlFlowAlg[Compiled] with NamespaceLookup with ModuleLookup {
  import io.dac.mara.ir.IrModel._

  override def ifelse(pred: Compiled, ifbody: Seq[Compiled], elsebody: Seq[Compiled]): Compiled = opWith[Typed] { typex =>
    val predCode = pred.fragment

    val resultType = MaraType.lower(typex).get

    val ifFragment = Compiled.recurse(ifbody)
    val ifResult = l(nextTemp())
    val ifType = MaraType.lower(ifbody.last.get[Typed]).get

    val elseFragment = Compiled.recurse(elsebody)
    val elseResult = l(nextTemp())
    val elseType = MaraType.lower(elsebody.last.get[Typed]).get

    val ifCode =
      label("ifLabel", "ifLabel:") ++
      ifFragment ++
      stmt("br label %endLabel")

    val elseCode =
      label("elseLabel", "elseLabel:") ++
      elseFragment ++
      stmt("br label %endLabel")

    val maybePhi: Fragment =
      if (elseFragment.isEmpty) Fragment.empty
      else stmt(l(nextTemp()), r(s"phi ${resultType} [${ifCode.result}, ${ifCode.block}], [${elseCode.result}, ${elseCode.block}]"))

    val endCode: Fragment =
      label("endLabel", "endLabel:") ++
      maybePhi

    (predCode :+
      stmt(s"br i1 ${predCode.result}, label ${ifCode.block}, label ${elseCode.block}")) ++
      ifCode ++
      elseCode ++
      endCode

  }
}
